package com.clinkle.sql

import Node._

object CREATE extends Composite(n"CREATE") { create =>
  object DATABASE extends Composite(create, n"DATABASE") with ApplyDatabaseName { parent: Node =>
    object IF_NOT_EXISTS extends Composite(parent, n"IF NOT EXISTS") with ApplyDatabaseName
  }

  trait ApplyDatabaseName { parent: Node =>
    case class DATABASE(database: Database) extends Composite(parent, database) with UpdateExec { parent: Node =>
      case class DEFAULT_CHARACTER_SET(characterSet: CharacterSet) extends Composite(parent, n"DEFAULT CHARACTER SET", characterSet) with UpdateExec { parent: Node =>
        case class DEFAULT_COLLATE(collate: Collate) extends Composite(parent, n"DEFAULT COLLATE", collate) with UpdateExec
      }
    }
    def apply(database: Database) = DATABASE(database)
  }

  object TABLE extends Composite(create, n"TABLE") with ApplyTable { parent: Node =>
    object IF_NOT_EXISTS extends Composite(parent, n"IF NOT EXISTS") with ApplyTable
  }

  trait ApplyTable { parent: Node =>
    case class TABLE(table: Table) extends Composite(parent, n"${ table.decl }") with UpdateExec
    def apply(table: Table) = TABLE(table)
  }

  trait CharacterSet extends Node
  object utf8mb4 extends Named with CharacterSet

  trait Collate extends Node
  object utf8mb4_unicode_ci extends Named with Collate
}
