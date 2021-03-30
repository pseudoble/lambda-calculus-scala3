package io.pseudoble.effects.typeclasses

trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A]
  def alt[A](fa: F[A])(fa2: F[A]): F[A]
}