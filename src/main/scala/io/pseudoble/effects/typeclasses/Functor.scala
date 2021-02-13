package io.pseudoble.effects.typeclasses

/** Functor Type Class */
trait Functor[F[_]]:

  /** lift - given a function A => B, return a new function F[A] => F[B] */
  def lift[A, B](f: => A => B): F[A] => F[B] = a => map(a)(f)

  extension[A, B] (fa: F[A])

  /** map - given a container with a value, apply a function to to the value and return a new container holding the 
   * result. */
    infix def map(f: => A => B): F[B]
