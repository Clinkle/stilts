package com.clinkle.sql

import java.sql.{ResultSet, Statement, Connection}

trait Execable[T] extends Node { query =>
  def exec(implicit executor: Executor): Stream[T]
  def tryFirst(implicit executor: Executor): Option[T] = exec.headOption
  def first(implicit executor: Executor): T = tryFirst.get
  def tryOnly(implicit executor: Executor): Option[T] = {
    val all = exec
    assert(all.isEmpty || all.tail.isEmpty)
    all.headOption
  }
  def only(implicit executor: Executor): T = tryOnly.get
}

trait UpdateExec extends Node { query =>
  def exec(implicit executor: Executor): Int = executor.executeUpdate(query)

  def updateAtMostOne(implicit executor: Executor): Int = {
    val numDeleted = exec
    assert(numDeleted <= 1, s"Query ${ query.serial } deleted more than one row.")
    numDeleted
  }

  def updateOne(implicit executor: Executor): Int = {
    val numDeleted = updateAtMostOne
    assert(numDeleted >= 1, s"Query ${ query.serial } deleted fewer than one row.")
    numDeleted
  }
}

trait OnDuplicateKeyUpdateExec extends Node { query =>
  // For normal inserts returns the number of rows inserted.
  // When used with `ON DUPLICATE KEY UPDATE` returns 1 if a new row was inserted and 2 if the row was updated.
  def exec(implicit executor: Executor): Int = executor.executeUpdate(query)
}

trait InsertExec extends OnDuplicateKeyUpdateExec { query =>
  def only(implicit executor: Executor): Int = {
    val inserted = exec
    assert(inserted == 1, s"Query ${ query.serial } inserted fewer or more than one row.")
    inserted
  }

  def insertAtMostOne(implicit executor: Executor): Int = {
    val inserted = exec
    assert(inserted <= 1, s"Query ${ query.serial } inserted $inserted rows.")
    inserted
  }

  def keys[T](implicit executor: Executor, primitive: Primitive[T]): Stream[T] = {
    val rows = executor.executeKeys(query)
    def next: Stream[T] = {
      if (!rows.next())
        Stream.empty
      else
        primitive.extract(rows, 1) #:: next
    }

    next
  }

  def key[T](implicit executor: Executor, primitive: Primitive[T]): T = {
    val allKeys = keys
    assert(allKeys.nonEmpty, s"Query ${ query.serial } did not insert any keys.")
    assert(allKeys.tail.isEmpty, s"Query ${ query.serial } inserted more than one key.")
    allKeys.head
  }
}

trait Executor {
  def executeQuery(query: Node): ResultSet
  def executeUpdate(query: Node): Int
  def executeKeys(query: Node): ResultSet
}

object Executor {
  implicit def ConnectionExecutor(implicit conn: Connection): Executor = new Executor {
    override def executeQuery(query: Node): ResultSet = {
      val sql = query.sql
      val stmt = conn.prepareStatement(sql)
      query.setParams(stmt, 1)
      stmt.executeQuery()
    }

    override def executeUpdate(query: Node): Int = {
      val sql = query.sql
      val stmt = conn.prepareStatement(sql, Statement.NO_GENERATED_KEYS)
      query.setParams(stmt, 1)
      stmt.executeUpdate()
    }

    override def executeKeys(query: Node): ResultSet = {
      val sql = query.sql
      val stmt = conn.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
      query.setParams(stmt, 1)
      stmt.executeUpdate()
      stmt.getGeneratedKeys
    }
  }
}
