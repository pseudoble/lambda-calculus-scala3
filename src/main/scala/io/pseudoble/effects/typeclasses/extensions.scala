package io.pseudoble.effects.typeclasses

import scala.annotation.targetName
import scala.util.chaining.scalaUtilChainingOps

object Extensions {

  /** Functor extensions 
   * - add 'map' functionality directly to F for more natural usage */
  extension [F[_], A, B](fa: F[A])(using functor: Functor[F])
    infix def map(f: A => B): F[B] =
      functor.fmap(fa)(f)
 
  /** Applicative extensions 
   * - add 'ap' functionality directly to F for more natural usage 
   * - add <*> operator for 'ap' 
   * - add 'product' and * product operator 
   * - add <*: and :*> operators (productL and productR respectively) */
  extension [F[_], A, B](ff: F[A => B])(using applicative: Applicative[F])
    /** Apply the function in F[A => B] to the value in F[A], resulting in F[B] */
    @targetName("ap") def <*>(fa: F[A]) = 
      applicative.ap(ff)(fa)

  extension [F[_], A, B](fa: F[A])(using applicative: Applicative[F])
    /** Merge F[A] and F[B] into F[(A, B)] */
    infix def product(fb: => F[B]): F[(A, B)] =
      // - The most natural implementation of product would use flatMap such as:
      //   for { a <- fa; b <- fb } yield (a, b)
      // However, not all Applicatives are Monads, so we don't have access to flatMap.
      // - This implementation uses Functor's map function and Applicative's apply (ap aka <*>) 
      // function to get the same result.
      // -To use Applicative's apply function, we must have an F containing a function and 
      // an F containing a value. We have two value-containing Fs. 
      // Here, we use Functor's map function to transform our F[A] into F[B => (A, B)]
      // Then we apply this to our F[B]. This application returns F[(A, B)] which is what 
      // we want.
      val fab = fa.map(a => { 
        (b: B) => (a, b)
      }) 
      fab <*> fb 

    def *(fb: F[B]): F[(A, B)] = 
      fa product fb

    @targetName("productL") 
    /** executes both and, if both succeed, returns only the left side */
    def <*:(fb: F[B]): F[A] = 
      product(fb).map(_._1) 

    @targetName("productR") 
    /** executes both and, if both succeed, returns only the right side */
    def :*>(fb: F[B]): F[B] = 
      product(fb).map(_._2) 

  extension [F[_], A](fa: F[A])(using alternative: Alternative[F])
    infix def orElse(other: => F[A]): F[A] =
      alternative.alt(fa)(other) 

    @targetName("alternative")
    def <|>(fa2: => F[A]): F[A] =
      orElse(fa2)
  extension [F[_], A, B](fa: F[A])(using monad: Monad[F])
    infix def flatMap(f: A => F[B]): F[B] =
      monad.flatMap(fa)(f)
    def >>=(f: A => F[B]): F[B] =
      monad.flatMap(fa)(f)

  extension [F[_], A](ffa: F[F[A]])(using Monad[F])
    def flatten: F[A] = ffa >>= identity

  extension [F[_], A, B](f: => A => F[B])(using Monad[F])
    def =<<:(fa: F[A]) = fa flatMap f
}