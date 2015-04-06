package com.clinkle.sql

import Node._

object ALTER extends Composite(n"ALTER") { alter =>
  case class TABLE[T <: Table](table: T) extends Composite(alter, n"TABLE", table) { alterTable =>
    object ADD extends Composite(alterTable, n"ADD") { add =>
      case class COLUMN[U](column: T#Column[U]) extends Composite(add, n"COLUMN ( `${ column.name }` ${ column.decl } )") with UpdateExec
    }

    object DROP extends Composite(alterTable, n"DROP") { drop =>
      case class COLUMN[U](column: T#Column[U]) extends Composite(drop, n"COLUMN ${ column.name }") with UpdateExec
    }

    object CHANGE extends Composite(alterTable, n"CHANGE") { change =>
      case class COLUMN[U](oldName: String, column: T#Column[U]) extends Composite(change, n"COLUMN `${ oldName }` `${ column.name }` ${ column.decl } ") with UpdateExec
    }

    object MODIFY extends Composite(alterTable, n"MODIFY") { change =>
      case class COLUMN[U](column: T#Column[U]) extends Composite(change, n"COLUMN `${ column.name }` ${ column.decl }") with UpdateExec
    }
  }
}
