package com.clinkle.sql

import com.clinkle.sql.Expr.OrderByable
import com.clinkle.sql.Node._

object DELETE extends Composite(n"DELETE") with DeleteNS.Ignore with DeleteNS.From

object DeleteNS {
  trait Ignore { parent: Node =>
    object IGNORE extends Composite(parent, n"IGNORE") with From
  }

  trait From { parent: Node =>
    case class FROM(n: Table) extends Composite(parent, n"FROM", n) with Where with OrderBy with Limit with UpdateExec
  }

  trait Where { parent: Node =>
    case class WHERE(predicate: Expr[Boolean]) extends Composite(parent, n"WHERE", predicate) with OrderBy with Limit with UpdateExec
  }

  trait OrderBy { parent: Node =>
    case class ORDER_BY(ns: OrderByable*) extends Composite(parent, n"ORDER BY", Composite(", ", ns:_*)) with Limit with UpdateExec
  }

  trait Limit { parent: Node =>
    case class LIMIT(count: Int) extends Composite(parent, n"LIMIT", Arg(count)) with UpdateExec
  }
}
