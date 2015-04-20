package com.clinkle.sql

import java.sql.Timestamp

import com.clinkle.sql.TypeConstraints.|∨|

import language.experimental.macros
import scala.collection.mutable

import Node._

trait Table extends Quoted with Comparable[Table] { table =>
  private var primaryKey: Option[Seq[Column[_]]] = None
  private def setPrimaryKey(cols: Column[_]*) = primaryKey match {
    case None => primaryKey = Some(cols)
    case Some(key) =>
      val existing = key.map(_.name).mkString("(", ", ", ")")
      val erroneous = cols.map(_.name).mkString("(", ", ", ")")
      throw new RuntimeException(s"Cannot make $erroneous the primary key. Table already has $existing as parimary.")
  }

  private val columns: mutable.LinkedHashMap[String, Column[_]] = mutable.LinkedHashMap()
  private val indexes: mutable.MutableList[String] = mutable.MutableList()
  private val foreignReferences: mutable.MutableList[Table] = mutable.MutableList()

  def column[T](name: String): this.Column[T] = new Column[T](name, "")

  class Column[T: Primitive](val name: String, val decl: String) extends Expr[T](n"${ table.sql }.`$name`") { column =>
    val parent = table
    columns.update(name, column)

    def alias: Alias[T] = macro Macros.alias_impl

    object AUTO_INCREMENT extends Column[T](name, s"$decl AUTO_INCREMENT")
    case class DEFAULT(v: T) extends Column[T](name, s"$decl DEFAULT ${ implicitly[Primitive[T]].toSerial(v) }")
    object NULL extends Column[Option[T]](name, s"$decl NULL")
    object NOT_NULL extends Column[T](name, s"$decl NOT NULL")
    def PRIMARY_KEY = {
      val pk = new Column[T](name, s"$decl PRIMARY KEY")
      setPrimaryKey(pk)
      pk
    }
    def UNIQUE_KEY = new Column[T](name, s"$decl UNIQUE KEY")
    def REFERENCES[U: (Option[T] |∨| T)#λ: Primitive](theirCol: Table#Column[U]) = {
      foreignReferences += theirCol.parent
      new Column[U](name, s"$decl REFERENCES ${ theirCol.parent.quotedName } (`${ theirCol.name }`)")
    }
  }

  import Primitive._

  protected def CHAR[T: StringPrimitive](length: Int): Column[T] = macro Macros.column_length
  protected def VARCHAR[T: StringPrimitive](length: Int): Column[T] = macro Macros.column_length
  protected def TEXT[T: StringPrimitive]: Column[T] = macro Macros.column
  protected def TINYINT[T: BytePrimitive]: Column[T] = macro Macros.column
  protected def SMALLINT[T: ShortPrimitive]: Column[T] = macro Macros.column
  protected def INT[T: IntPrimitive]: Column[T] = macro Macros.column
  protected def BIGINT[T: LongPrimitive]: Column[T] = macro Macros.column
  protected def BOOL[T: BooleanPrimitive]: Column[T] = macro Macros.column
  protected def FLOAT[T: FloatPrimitive]: Column[T] = macro Macros.column
  protected def DOUBLE[T: DoublePrimitive]: Column[T] = macro Macros.column
  protected def BLOB(implicit primitive: Primitive[Array[Byte]]): Column[Array[Byte]] = macro Macros.column
  protected def MEDIUMBLOB(implicit primitive: Primitive[Array[Byte]]): Column[Array[Byte]] = macro Macros.column
  protected def TIMESTAMP(implicit primitive: Primitive[Timestamp]): Column[Timestamp] = macro Macros.column

  private def addIndexDecl(prefix: String, cols: Seq[Column[_]]): Unit =
    indexes += s"$prefix (${ cols.map(col => s"`${ col.name }`").mkString(", ") })"

  protected def PRIMARY_KEY(cols: Column[_]*): Unit = {
    setPrimaryKey(cols:_*)
    addIndexDecl("PRIMARY KEY", cols)
  }

  protected def INDEX(cols: Column[_]*): Unit = addIndexDecl("INDEX", cols)

  protected def UNIQUE_KEY(cols: Column[_]*): Unit = addIndexDecl("UNIQUE KEY", cols)

  protected case class FOREIGN_KEY[X, Y: (Option[X] |∨| X)#λ](myCol: Column[Y]) {
    def REFERENCES[T: (Option[X] |∨| X)#λ](theirCol: Table#Column[T]): Unit = {
      foreignReferences += theirCol.parent
      indexes += s"FOREIGN KEY (`${ myCol.name }`) REFERENCES ${ theirCol.parent.quotedName } (`${ theirCol.name }`)"
    }
  }

  def decl: String = s"$quotedName (\n${
    val cols = columns.map(_._2).map(col => s"`${ col.name }` ${ col.decl }")
    (cols ++ indexes).mkString(",\n")
  }\n) ENGINE=InnoDB"

  final override def compareTo(that: Table): Int = {
    if (that eq this) return 0

    if (that.foreignReferences.contains(this)) -1
    else if (this.foreignReferences.contains(that)) 1
    else 0
  }
}

object Table {
  // Table A < Table B iff Table B contains a foreign key reference to a column in Table A. You can use this ordering to ensure
  // that a table creation statement is not executed before the foreign table it refers to has been defined.
  implicit object TablesArePartiallyOrdered extends PartialOrdering[Table] {
    override def tryCompare(x: Table, y: Table): Option[Int] =
      if (x eq y)
        Some(0)
      else if (y.foreignReferences.contains(x))
        Some(-1)
      else if (x.foreignReferences.contains(y))
        Some(1)
      else
        None

    override def lteq(x: Table, y: Table): Boolean =
      tryCompare(x, y).fold(false)(_ <= 0)
  }
}


class Alias[T: Primitive](name: String) extends Expr[T](n"`$name`")

case class As[T: Primitive](expr: Expr[T], alias: Alias[T]) extends Expr[T](expr, n"AS", alias)
