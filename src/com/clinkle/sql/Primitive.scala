package com.clinkle.sql

import java.sql.{Timestamp, ResultSet, PreparedStatement}

trait Primitive[T] {
  def setParam(stmt: PreparedStatement, index: Int, value: T)
  def extract(rows: ResultSet, index: Int): T
  def toSerial(value: T): String = value.toString
}

object Primitive {
  trait StringPrimitive[T] extends Primitive[T]
  trait BooleanPrimitive[T] extends Primitive[T]
  trait NullablePrimitive[T] extends Primitive[T]

  trait NumericPrimitive[T] extends Primitive[T]
  trait BytePrimitive[T] extends NumericPrimitive[T]
  trait ShortPrimitive[T] extends NumericPrimitive[T]
  trait IntPrimitive[T] extends NumericPrimitive[T]
  trait LongPrimitive[T] extends NumericPrimitive[T]
  trait DoublePrimitive[T] extends NumericPrimitive[T]
  trait FloatPrimitive[T] extends NumericPrimitive[T]

  implicit object ByteIsPrimitive extends BytePrimitive[Byte] {
    def setParam(stmt: PreparedStatement, index: Int, value: Byte) = stmt.setByte(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getByte(index)
  }

  implicit object ShortIsPrimitive extends ShortPrimitive[Short] {
    def setParam(stmt: PreparedStatement, index: Int, value: Short) = stmt.setShort(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getShort(index)
  }

  implicit object IntIsPrimitive extends IntPrimitive[Int] {
    def setParam(stmt: PreparedStatement, index: Int, value: Int) = stmt.setInt(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getInt(index)
  }

  implicit object LongIsPrimitive extends LongPrimitive[Long] {
    def setParam(stmt: PreparedStatement, index: Int, value: Long) = stmt.setLong(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getLong(index)
  }

  implicit object DoubleIsPrimitive extends DoublePrimitive[Double] {
    def setParam(stmt: PreparedStatement, index: Int, value: Double) = stmt.setDouble(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getDouble(index)
  }

  implicit object FloatIsPrimitive extends FloatPrimitive[Float] {
    def setParam(stmt: PreparedStatement, index: Int, value: Float) = stmt.setFloat(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getFloat(index)
  }

  implicit object StringIsPrimitive extends StringPrimitive[String] {
    def setParam(stmt: PreparedStatement, index: Int, value: String) = stmt.setString(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getString(index)
    override def toSerial(value: String) = s""""$value""""
  }

  implicit object BooleanIsPrimitive extends BooleanPrimitive[Boolean] {
    def setParam(stmt: PreparedStatement, index: Int, value: Boolean) = stmt.setBoolean(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getBoolean(index)
    override def toSerial(value: Boolean) = value.toString.toUpperCase
  }

  implicit object ByteArrayIsPrimitive extends Primitive[Array[Byte]] {
    def setParam(stmt: PreparedStatement, index: Int, value: Array[Byte]) = stmt.setBytes(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getBytes(index)
  }

  implicit object TimestampIsPrimitive extends Primitive[Timestamp] {
    def setParam(stmt: PreparedStatement, index: Int, value: Timestamp) = stmt.setTimestamp(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getTimestamp(index)
  }

  implicit object AnyIsPrimitive extends Primitive[Any] {
    def setParam(stmt: PreparedStatement, index: Int, value: Any) = stmt.setObject(index, value)
    def extract(rows: ResultSet, index: Int) = rows.getObject(index)
  }

  implicit def optionIsPrimitive[T: Primitive] = new NullablePrimitive[Option[T]] {
    def setParam(stmt: PreparedStatement, index: Int, value: Option[T]) = value match {
      case Some(v) => implicitly[Primitive[T]].setParam(stmt, index, v)
      case None => stmt.setObject(index, null)
    }

    def extract(rows: ResultSet, index: Int) =
      if ({ rows.getObject(index); rows.wasNull() })
        None
      else
        Some(implicitly[Primitive[T]].extract(rows, index))

    override def toSerial(value: Option[T]) = value match {
      case None => "NULL"
      case Some(x) => implicitly[Primitive[T]].toSerial(x)
    }
  }
}
