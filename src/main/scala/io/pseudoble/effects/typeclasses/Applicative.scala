package io.pseudoble.effects.typeclasses

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def ap[A,B](ff: F[A => B])(fa: F[A]): F[B]
}

