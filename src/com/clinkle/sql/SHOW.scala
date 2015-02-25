package com.clinkle.sql

import Node._

object SHOW extends Composite(n"SHOW") { show =>
  object TABLES extends Composite(show, n"TABLES") { query =>
    def exec(implicit executor: Executor): Stream[String] = {
      val rows = executor.executeQuery(query)
        def next: Stream[String] =
          if (!rows.next())
            Stream.empty
          else
            rows.getString(1) #:: next

        next
    }
  }

  object CREATE extends Composite(show, n"CREATE") { create =>
    trait ShowExec extends Execable[(String, String)] { query: Node =>
      override def exec(implicit executor: Executor): Stream[(String, String)] = {
        val rows = executor.executeQuery(query)
        def next: Stream[(String, String)] =
          if (!rows.next())
            Stream.empty
          else
            (rows.getString(1), rows.getString(2)) #:: next

        next
      }
    }

    case class TABLE(tab: Table) extends Composite(create, n"TABLE", tab) with ShowExec
    case class DATABASE(database: Database) extends Composite(create, n"DATABASE", database) with ShowExec
  }
}
