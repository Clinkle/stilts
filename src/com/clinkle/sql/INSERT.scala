package com.clinkle.sql

import language.experimental.macros
import com.clinkle.sql.Expr.Assoc
import com.clinkle.sql.Node._

object INSERT extends Composite(n"INSERT") with InsertNS.Ignore with InsertNS.Into

object InsertNS {
  trait Ignore { parent: Node =>
    object IGNORE extends Composite(parent, n"IGNORE") with Into
  }

  trait Into { parent: Node =>
    case class INTO(table: Table) extends Composite(parent, n"INTO", table) with Columns with Set
  }

  trait Columns { parent: Node =>
    case class Columns[T <: Product](cols: Table#Column[_]*) extends Composite(parent, n"(", Composite(", ", cols:_*), n")") { columnsNode =>
      case class VALUES(tups: T*) extends Composite(columnsNode, n"VALUES", Composite(", ", tups.map( tup =>
        Composite(n"(", Composite(", ", tup.productIterator.toSeq.asInstanceOf[Seq[Expr[_]]]:_*), n")")
      ):_*)) with OnDuplicateKeyUpdate with InsertExec
    }

    def apply[A](col0: Table#Column[A]) = Columns[Tuple1[Expr[A]]](col0)
    def apply[A, B](col0: Table#Column[A], col1: Table#Column[B]) = Columns[(Expr[A], Expr[B])](col0, col1)
    def apply[A, B, C](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C]) = Columns[(Expr[A], Expr[B], Expr[C])](col0, col1, col2)
    def apply[A, B, C, D](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D])](col0, col1, col2, col3)
    def apply[A, B, C, D, E](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E])](col0, col1, col2, col3, col4)
    def apply[A, B, C, D, E, F](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F])](col0, col1, col2, col3, col4, col5)
    def apply[A, B, C, D, E, F, G](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G])](col0, col1, col2, col3, col4, col5, col6)
    def apply[A, B, C, D, E, F, G, H](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H])](col0, col1, col2, col3, col4, col5, col6, col7)
    def apply[A, B, C, D, E, F, G, H, I](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I])](col0, col1, col2, col3, col4, col5, col6, col7, col8)
    def apply[A, B, C, D, E, F, G, H, I, J](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9)
    def apply[A, B, C, D, E, F, G, H, I, J, K](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q], col17: Table#Column[R]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q], col17: Table#Column[R], col18: Table#Column[S]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17, col18)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q], col17: Table#Column[R], col18: Table#Column[S], col19: Table#Column[T]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17, col18, col19)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q], col17: Table#Column[R], col18: Table#Column[S], col19: Table#Column[T], col20: Table#Column[U]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17, col18, col19, col20)
    def apply[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](col0: Table#Column[A], col1: Table#Column[B], col2: Table#Column[C], col3: Table#Column[D], col4: Table#Column[E], col5: Table#Column[F], col6: Table#Column[G], col7: Table#Column[H], col8: Table#Column[I], col9: Table#Column[J], col10: Table#Column[K], col11: Table#Column[L], col12: Table#Column[M], col13: Table#Column[N], col14: Table#Column[O], col15: Table#Column[P], col16: Table#Column[Q], col17: Table#Column[R], col18: Table#Column[S], col19: Table#Column[T], col20: Table#Column[U], col21: Table#Column[V]) = Columns[(Expr[A], Expr[B], Expr[C], Expr[D], Expr[E], Expr[F], Expr[G], Expr[H], Expr[I], Expr[J], Expr[K], Expr[L], Expr[M], Expr[N], Expr[O], Expr[P], Expr[Q], Expr[R], Expr[S], Expr[T], Expr[U], Expr[V])](col0, col1, col2, col3, col4, col5, col6, col7, col8, col9, col10, col11, col12, col13, col14, col15, col16, col17, col18, col19, col20, col21)
  }

  trait Set { parent: Node =>
    case class SET(ns: Assoc*) extends Composite(parent, n"SET", Composite(", ", ns:_*)) with OnDuplicateKeyUpdate with InsertExec
  }

  trait OnDuplicateKeyUpdate { parent: Node =>
    case class ON_DUPLICATE_KEY_UPDATE(ns: Assoc*) extends Composite(parent, n"ON DUPLICATE KEY UPDATE", Composite(", ", ns:_*)) with OnDuplicateKeyUpdateExec
  }
}
