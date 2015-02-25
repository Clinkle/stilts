package com.clinkle.sql

object TypeConstraints {
  // Implementation of type disjunction adapted from http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/ as
  // found via http://stackoverflow.com/questions/3508077/how-to-define-type-disjunction-union-types
  type ¬[A] = A => Nothing
  type ∨[T, U] = ¬[¬[T] with ¬[U]]
  type ¬¬[A] = ¬[¬[A]]
  type |∨|[T, U] = { type λ[X] = ¬¬[X] <:< (T ∨ U) }
}
