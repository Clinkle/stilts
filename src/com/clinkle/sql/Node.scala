package com.clinkle.sql

import java.sql.PreparedStatement

// Any object that can be serialized to a piece of SQL.

trait Node { query =>
  def sql: String
  def setParams(stmt: PreparedStatement, startIndex: Int): Int

  def serial: String
  def getParams: List[Any]
  final def print: this.type = { println(query.serial); this }
}

object Node {
  sealed class Fixed(name: String) extends Node {
    final def sql: String = name
    final def setParams(stmt: PreparedStatement, startIndex: Int): Int = startIndex
    final def serial: String = sql
    final def getParams: List[Any] = List()
  }

  implicit class FixedNode(sc: StringContext) {
    def n(args: Any*): Fixed = new Fixed(sc.s(args:_*))
  }

  class Arg[T: Primitive](v: T) extends Node {
    final def sql: String = "?"
    final def setParams(stmt: PreparedStatement, startIndex: Int): Int = {
      implicitly[Primitive[T]].setParam(stmt, startIndex, v)
      startIndex + 1
    }

    final def serial: String = implicitly[Primitive[T]].toSerial(v)
    final def getParams: List[Any] = List(v)
  }
  object Arg { def apply[T: Primitive](v: T) = new Arg(v) }

  class Composite(sep: String, ns: Node*) extends Node {
    def this(ns: Node*) = this(" ", ns:_*)

    final def sql: String = ns.map(_.sql).mkString(sep)
    final def setParams(stmt: PreparedStatement, startIndex: Int): Int = ns.foldLeft(startIndex)((i, n) => n.setParams(stmt, i))
    final def serial: String = ns.map(_.serial).mkString(sep)
    final def getParams: List[Any] = ns.flatMap(_.getParams).toList
  }
  object Composite {
    def apply(ns: Node*) = new Composite(ns:_*)
    def apply(sep: String, ns: Node*) = new Composite(sep, ns:_*)
  }

  trait Named extends Node {
    val bareName = this.getClass.getName.replace('.', '$').split('$').last.stripSuffix("$")
    def sql: String = bareName
    def serial: String = sql
    def setParams(stmt: PreparedStatement, startIndex: Int): Int = startIndex
    def getParams = List()
  }

  trait Quoted extends Named {
    val quotedName = s"`$bareName`"
    override def sql: String = quotedName
  }
}
