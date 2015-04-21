package com.clinkle.sql

import com.clinkle.sql.Expr.{OrderByable, Assoc}
import com.clinkle.sql.Node._

object UPDATE extends Composite(n"UPDATE") with UpdateNS.ApplyUpdate with UpdateNS.Ignore

object UpdateNS {
  trait Ignore { parent: Node =>
    object IGNORE extends Composite(parent, n"IGNORE") with ApplyUpdate
  }

  trait ApplyUpdate { parent: Node =>
    case class UPDATE(tableRef: Node) extends Composite(parent, tableRef) with Join with Set
    def apply(tableRef: Table) = UPDATE(tableRef)
  }

  trait Join { parent: Node =>
    case class INNER_JOIN(n: Node) extends Composite(parent, n"INNER JOIN", n)
    case class LEFT_JOIN(n: Node) extends Composite(parent, n"LEFT JOIN", n)
  }

  trait On { parent: Node =>
    case class ON(n: Node) extends Composite(parent, n"ON", n) with Set
  }

  trait Set { parent: Node =>
    case class SET(n1: Assoc, ns: Assoc*) extends Composite(parent, n"SET", Composite(", ", (n1 +: ns):_*)) with Where with OrderBy with Limit with UpdateExec
  }

  trait Where { parent: Node =>
    case class WHERE(expr: Expr[Boolean]) extends Composite(parent, n"WHERE", expr) with OrderBy with Limit with UpdateExec
  }

  trait OrderBy { parent: Node =>
    case class ORDER_BY(exprs: OrderByable*) extends Composite(parent, n"ORDER BY", Composite(exprs:_*)) with Limit with UpdateExec
  }

  trait Limit { parent: Node =>
    case class LIMIT(count: Int) extends Composite(parent, n"LIMIT", Node.Arg(count)) with UpdateExec
  }
}
