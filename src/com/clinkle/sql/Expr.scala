package com.clinkle.sql

import java.sql.{Timestamp, ResultSet}

import com.clinkle.sql.Primitive.{NullablePrimitive, StringPrimitive, BooleanPrimitive, NumericPrimitive}
import Node._

class Expr[T: Primitive](ns: Node*) extends Composite(ns:_*) {
  final def extract(rows: ResultSet, index: Int): T = implicitly[Primitive[T]].extract(rows, index)

  def AS(alias: Alias[T]) = As(this, alias)
}

object Expr {
  def apply[T: Primitive](ns: Node*) = new Expr[T](ns:_*)

  implicit class Arg[T: Primitive](arg: T) extends Expr[T](Node.Arg(arg))

  implicit class NumericExpr[T: NumericPrimitive](one: Expr[T]) {
    def +[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"+", two, n")")
    def -[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"-", two, n")")
    def *[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"*", two, n")")
    def /[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"/", two, n")")
    def %[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"%", two, n")")
    def DIV[U: NumericPrimitive](two: Expr[U]) = Expr[T](n"(", one, n"DIV", two, n")")
    def unary_- = Expr[T](n"-", one)
  }

  implicit class BooleanExpr[T: BooleanPrimitive](one: Expr[T]) {
    def AND[U: BooleanPrimitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"AND", two, n")")
    def OR[U: BooleanPrimitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"OR", two, n")")
    def XOR[U: BooleanPrimitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"XOR", two, n")")
  }

  implicit class StringExpr[T: StringPrimitive](one: Expr[T]) {
    def LIKE[U: StringPrimitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"LIKE", two, n")")
    def RLIKE[U: StringPrimitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"RLIKE", two, n")")
  }

  implicit class ComparableExpr[T: Primitive](one: Expr[T]) {
    def ===[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"=", two, n")")
    def <=>[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"<=>", two, n")")
    def !==[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"!=", two, n")")
    def <[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"<", two, n")")
    def >[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n">", two, n")")
    def <=[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n"<=", two, n")")
    def >=[U: Primitive](two: Expr[U]) = Expr[Boolean](n"(", one, n">=", two, n")")
  }

  implicit class NullableExpr[T: Primitive](one: Expr[T]) {
    def IS_NULL = Expr[Boolean](one, n"IS NULL")
    def IS_NOT_NULL = Expr[Boolean](one, n"IS NOT NULL")
  }

  trait OrderByable extends Node
  implicit class OrderByableWrapper(expr: Expr[_]) extends Composite(expr, n"ASC") with OrderByable {
    case object ASC extends Composite(expr, n"ASC") with OrderByable
    case object DESC extends Composite(expr, n"DESC") with OrderByable
  }

  trait Assoc extends Node
  implicit class AssocWrapper[T: Primitive](col: Table#Column[T]) {
    case class :=(v: Expr[T]) extends Composite(col, n"=", v) with Assoc
  }

  // Global functions.

  object *
  def COUNT(a: *.type) = Expr[Int](n"COUNT(*)")

  def COUNT[T: Primitive](a: Expr[T]) = Expr[Int](n"COUNT(", a, n")")
  def SUM[T: Primitive](a: Expr[T]) = Expr[Option[T]](n"SUM(", a, n")")
  def AVG[T: Primitive](a: Expr[T]) = Expr[T](n"AVG(", a, n")")
  def MAX[T: Primitive](a: Expr[T]) = Expr[Option[T]](n"MAX(", a, n")")
  def MIN[T: Primitive](a: Expr[T]) = Expr[Option[T]](n"MIN(", a, n")")
  def LENGTH[T: Primitive](a: Expr[T]) = Expr[Int](n"LENGTH(", a, n")")
  def GREATEST[T: Primitive](exprs: Expr[T]*) = Expr[T](n"GREATEST(", Composite(", ", exprs:_*), n")")
  def LEAST[T: Primitive](exprs: Expr[T]*) = Expr[T](n"LEAST(", Composite(", ", exprs: _*), n")")
  def FROM_UNIXTIME[T: NumericPrimitive](a: Expr[T]) = Expr[Timestamp](n"FROM_UNIXTIME(", a, n")")

  def STRCMP[T: StringPrimitive, U: StringPrimitive](a: Expr[T], b: Expr[U]) = Expr[Int](n"STRCMP(", a, n",", b, n")")
  def FIND_IN_SET[T: StringPrimitive, U: StringPrimitive](a: Expr[T], b: Expr[U]) = Expr[Int](n"FIND_IN_SET(", a, n",", b, n")")
  def SUBSTRING[T: StringPrimitive, U: NumericPrimitive](a: Expr[T], b: Expr[U]) = Expr[T](n"SUBSTRING(", a, n",", b, n")")
  def CONCAT[T: StringPrimitive, U: StringPrimitive](a: Expr[T], b: Expr[U]) = Expr[T](n"CONCAT(", a, n",", b, n")")
  def ASCII[T: StringPrimitive](a: Expr[T]) = Expr[Int](n"ASCII(", a, n")")

  def NOT[T: BooleanPrimitive](a: Expr[T]) = Expr[Boolean](n"NOT(", a, n")")

  def ISNULL[T: NullablePrimitive](a: Expr[T]) = Expr[Boolean](n"ISNULL(", a, n")")
  def COALESCE[T: NullablePrimitive](exprs: Expr[T]*) = Expr[T](n"COALESCE(", Composite(", ", exprs:_*), n")")

  implicit class InExpr[T: Primitive](a: Expr[T]) {
    def IN(set: Expr[T]*): Expr[Boolean] = Expr[Boolean](a, n"IN (", Composite(", ", set:_*), n")")
    def IN(set: Set[T]): Expr[Boolean] = IN(set.map(new Arg(_)).toSeq:_*)

    def NOT_IN(set: Expr[T]*) = Expr[Boolean](a, n"NOT IN (", Composite(", ", set:_*), n")")
    def NOT_IN(set: Set[T]): Expr[Boolean] = NOT_IN(set.map(new Arg(_)).toSeq:_*)
  }

  trait Type[T] extends Node
  object UNSIGNED extends Composite(n"UNSIGNED") with Type[Int]
  def CONVERT[T: Primitive, U: Primitive](a: Expr[T], tpe: Type[U]) = Expr[U](n"CONVERT(", a, n",", tpe, n")")

  def WRAP[T: Primitive](a: Expr[T]) = Expr[Option[T]](a)
  def UNWRAP[T: Primitive](a: Expr[Option[T]]) = Expr[T](a)

  case class CASE[T](implicit primitive: Primitive[T]) extends Composite(n"CASE") { cse =>
    case class WHEN(predicate: Expr[Boolean], parent: Node = cse) { whn =>
      case class THEN(expr: Expr[T]) extends Composite(parent, n"WHEN", predicate, n"THEN", expr) { thn =>
        def WHEN(predicate: Expr[Boolean]) = cse.WHEN(predicate, thn)
        case class ELSE(expr: Expr[T]) { els =>
          object END extends Expr[T](thn, n"ELSE", expr, n"END")
        }
      }
    }
  }
}
