package io.pseudoble.effects.typeclasses

import io.pseudoble.effects.typeclasses.Applicative

/** Monad Type Class */
trait Monad[F[_]] extends Applicative[F]:
  extension [A,B](fa: F[A])
  /** flatMap - mapping over A => F[B] would yield F[F[B]]. flatMap is equivalent to mapping over this function and 
   *            then flattening the result to F[B]. */
    infix def flatMap(f: => A => F[B]): F[B]
    def >>=(f: => A => F[B]) = fa flatMap f

    /** const - FlatMap F[A] with F[B], but discard F[A] and return F[B] 
     * todo: Isn't this equivalent to productR ? */
    infix def const(fb: => F[B]): F[B] = fa >>= (_ => fb)
    def >>(fb: => F[B]) = fa const fb

    /** map 
     * implemented in terms of flatmap and pure */
    override def map(f: => A => B): F[B] = fa >>= f andThen pure

    /** applicative apply
     * implemented in terms of flatmap and map */
    override def ap(f: => F[A => B]): F[B] = fa >>= (a => f map (ff => ff(a)))

  /** flatten - given a container nested within a container, flatten to a single layer. */
  extension [A](ffa: => F[F[A]]) def flatten: F[A] = ffa >>= identity

  /** flatmap right associative operator */
  extension [A,B](f: => A => F[B]) def =<<:(fa: F[A]) = fa flatMap f

  /** const right associative operator */
  extension [A,B](fb: F[B]) def <<:(fa: => F[A]) = fa const fb