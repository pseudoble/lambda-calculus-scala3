package io.pseudoble.effects.typeclasses

trait Functor[F[_]] {
  def fmap[A,B](fa: F[A])(f: A => B): F[B]
  def lift[A, B](f: A => B): F[A] => F[B] = fa => fmap(fa)(f) 
}