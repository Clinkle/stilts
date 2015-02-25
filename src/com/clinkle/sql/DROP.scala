package com.clinkle.sql

import com.clinkle.sql.Node._

object DROP extends Composite(n"DROP") { drop =>
  object DATABASE extends Composite(drop, n"DATABASE") with ApplyDatabase { parent: Node =>
    object IF_EXISTS extends Composite(parent, n"IF EXISTS") with ApplyDatabase
  }

  trait ApplyDatabase { parent: Node =>
    case class DATABASE(database: Database) extends Composite(parent, database) with UpdateExec
    def apply(database: Database) = DATABASE(database)
  }

  object TABLE extends Composite(drop, n"TABLE") with ApplyTable { parent: Node =>
    object IF_EXISTS extends Composite(parent, n"IF EXISTS") with ApplyTable
  }

  trait ApplyTable { parent: Node =>
    case class TABLE(table: Table) extends Composite(parent, table) with UpdateExec
    def apply(table: Table) = TABLE(table)
  }
}
