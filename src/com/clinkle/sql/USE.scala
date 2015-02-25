package com.clinkle.sql

import com.clinkle.sql.Node._

case class USE(database: Database) extends Composite(n"USE", database) { query =>
  def exec(implicit executor: Executor): Unit = executor.executeQuery(query)
}
