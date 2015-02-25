package com.clinkle.sql

import Node._
import com.clinkle.sql.Expr.OrderByable

trait Select extends Node { parent =>
  class SELECT[T](exprs: Expr[_]*) {
    trait From extends Node { parent =>
      class FROM private (n: Node) extends Composite(parent, n"FROM", n)
        with Join with Where with GroupBy with Having with OrderBy with Limit with ForUpdate with Union with Exec

      object FROM {
        private val nextAlias = Iterator.from(0)

        def apply(table: Table) = new FROM(table)
        def apply(subquery: SELECT.SELECT[T]#Exec) = new FROM(Composite(n"(", subquery, n") AS alias_${
          nextAlias.synchronized(nextAlias.next())
        }"))
      }
    }

    trait Join extends Node { parent =>
      case class INNER_JOIN(n: Node) extends Composite(parent, n"INNER JOIN", n) with On
      case class LEFT_JOIN(n: Node) extends Composite(parent, n"LEFT JOIN", n) with On
    }

    trait On extends Node { parent =>
      case class ON(n: Node) extends Composite(parent, n"ON", n)
        with Join with Where with GroupBy with Having with OrderBy with Limit with ForUpdate with Union with Exec
    }

    trait Where extends Node { parent =>
      case class WHERE(n: Expr[Boolean]) extends Composite(parent, n"WHERE", n)
        with GroupBy with Having with OrderBy with Limit with ForUpdate with Union with Exec
    }

    trait GroupBy extends Node { parent =>
      case class GROUP_BY(n: Node) extends Composite(parent, n"GROUP BY", n)
        with Having with OrderBy with Limit with ForUpdate with Union with Exec
    }

    trait Having extends Node { parent =>
      case class HAVING(n: Node) extends Composite(parent, n"HAVING", n)
        with OrderBy with Limit with ForUpdate with Union with Exec
    }

    trait OrderBy extends Node { parent =>
      case class ORDER_BY(ns: OrderByable*) extends Composite(parent, n"ORDER BY", Composite(", ", ns:_*))
        with Limit with ForUpdate with Union with Exec
    }

    trait Limit extends Node { parent =>
      sealed class LIMIT(ns: Node*) extends Composite(parent, n"LIMIT", Composite(", ", ns:_*)) with ForUpdate with Union with Exec

      def LIMIT(start: Int, count: Int) = new LIMIT(Arg(start), Arg(count))
      def LIMIT(count: Int) = new LIMIT(Arg(count))
    }

    trait ForUpdate extends Node { parent =>
      case object FOR_UPDATE extends Composite(parent, n"FOR UPDATE") with Exec
    }

    trait Union extends Node { parent =>
      case class UNION(other: SELECT[T]#Union) extends Composite(parent, n"UNION", other) with OrderBy with Limit with Union with Exec
    }

    trait Exec extends Node with Execable[T] { query =>
      def exec(implicit executor: Executor): Stream[T] = {
        val rows = executor.executeQuery(query)
        def next: Stream[T] =
          if (!rows.next())
            Stream.empty
          else {
            val a = exprs.zipWithIndex.map { case (extr, i) => extr.extract(rows, i + 1) }
            seqToTuple(a).asInstanceOf[T] #:: next
          }

        next
      }
    }

    object N extends Composite(parent, Composite(", ", exprs:_*)) with From with Exec
  }

  def apply[A](a: Expr[A]) = new SELECT[A](a).N
  def apply[A, B](a: Expr[A], b: Expr[B]) = new SELECT[(A, B)](a, b).N
  def apply[A, B, C](a: Expr[A], b: Expr[B], c: Expr[C]) = new SELECT[(A, B, C)](a, b, c).N
  def apply[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D]) = new SELECT[(A, B, C, D)](a, b, c, d).N
  def apply[A, B, C, D, E](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E]) = new SELECT[(A, B, C, D, E)](a, b, c, d, e).N
  def apply[A, B, C, D, E, F](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F]) = new SELECT[(A, B, C, D, E, F)](a, b, c, d, e, f).N
  def apply[A, B, C, D, E, F, G](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G]) = new SELECT[(A, B, C, D, E, F, G)](a, b, c, d, e, f, g).N
  def apply[A, B, C, D, E, F, G, H](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H]) = new SELECT[(A, B, C, D, E, F, G, H)](a, b, c, d, e, f, g, h).N
  def apply[A, B, C, D, E, F, G, H, I](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I]) = new SELECT[(A, B, C, D, E, F, G, H, I)](a, b, c, d, e, f, g, h, i).N
  def apply[A, B, C, D, E, F, G, H, I, J](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J]) = new SELECT[(A, B, C, D, E, F, G, H, I, J)](a, b, c, d, e, f, g, h, i, j).N
  def apply[A, B, C, D, E, F, G, H, I, J, K](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K)](a, b, c, d, e, f, g, h, i, j, k).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L)](a, b, c, d, e, f, g, h, i, j, k, l).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M)](a, b, c, d, e, f, g, h, i, j, k, l, m).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)](a, b, c, d, e, f, g, h, i, j, k, l, m, n).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u).N
  def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](a: Expr[A], b: Expr[B], c: Expr[C], d: Expr[D], e: Expr[E], f: Expr[F], g: Expr[G], h: Expr[H], i: Expr[I], j: Expr[J], k: Expr[K], l: Expr[L], m: Expr[M], n: Expr[N], o: Expr[O], p: Expr[P], q: Expr[Q], r: Expr[R], s: Expr[S], t: Expr[T], u: Expr[U], v: Expr[V]) = new SELECT[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v).N

  private def seqToTuple(out: Seq[Any]) = out match {
    case Seq(a) => a
    case Seq(a, b) => (a, b)
    case Seq(a, b, c) => (a, b, c)
    case Seq(a, b, c, d) => (a, b, c, d)
    case Seq(a, b, c, d, e) => (a, b, c, d, e)
    case Seq(a, b, c, d, e, f) => (a, b, c, d, e, f)
    case Seq(a, b, c, d, e, f, g) => (a, b, c, d, e, f, g)
    case Seq(a, b, c, d, e, f, g, h) => (a, b, c, d, e, f, g, h)
    case Seq(a, b, c, d, e, f, g, h, i) => (a, b, c, d, e, f, g, h, i)
    case Seq(a, b, c, d, e, f, g, h, i, j) => (a, b, c, d, e, f, g, h, i, j)
    case Seq(a, b, c, d, e, f, g, h, i, j, k) => (a, b, c, d, e, f, g, h, i, j, k)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l) => (a, b, c, d, e, f, g, h, i, j, k, l)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    case Seq(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  }
}

object SELECT extends Composite(n"SELECT") with Select { parent =>
  object DISTINCT extends Composite(parent, n"DISTINCT") with Select
}
