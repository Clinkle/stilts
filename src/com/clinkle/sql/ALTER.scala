package com.clinkle.sql

import Node._

object ALTER extends Composite(n"ALTER") { alter =>
  case class TABLE[T <: Table](table: T) extends Composite(alter, n"TABLE", table) { alterTable =>
    object ADD extends Composite(alterTable, n"ADD") { add =>
      case class COLUMN(columns: T#Column[_]*) extends Composite(add, n"COLUMN (",
        Composite(", ", columns.map(column => n"`${ column.name }` ${ column.decl }"):_*),  n")") with UpdateExec
    }

    object DROP extends Composite(alterTable, n"DROP") { drop =>
      case class COLUMN(column: T#Column[_]) extends Composite(drop, n"COLUMN ${ column.name }") with UpdateExec
    }

    object CHANGE extends Composite(alterTable, n"CHANGE") { change =>
      case class COLUMN(oldName: String, column: T#Column[_]) extends Composite(change, n"COLUMN `${ oldName }` `${ column.name }` ${ column.decl } ") with UpdateExec
    }

    object MODIFY extends Composite(alterTable, n"MODIFY") { change =>
      case class COLUMN(column: T#Column[_]) extends Composite(change, n"COLUMN `${ column.name }` ${ column.decl }") with UpdateExec
    }
  }
}
