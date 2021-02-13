package io.pseudoble.effects.typeclasses

import scala.annotation.targetName
import io.pseudoble.effects.typeclasses.Functor


/** Applicative Type Class */
trait Applicative[F[_]] extends Functor[F]:
  
  /** pure - given a value, wrap it in a container and return it. */
  def pure[A](x: => A): F[A]
  
  extension [A, B](fa: F[A])  
    /** ap - given a container with a value A and another container with a function A => B, apply the function
     *       from the second container to the value in the first container and return a new container holding the
     *       result. */
    infix def ap(ff: => F[A => B]): F[B]
    @targetName("application") def <*>(ff: => F[A => B]) = fa ap ff

    /** product - combine two containers into one with values of each paired as a tuple 
     *            aka, create a higher order tuple */
    infix def product(fb: => F[B]): F[(A, B)] =
      fa <*> fb.map(b => a => (a, b))
    infix def *(fb: => F[B]): F[(A, B)] =
      fa product fb
      
    /** productR - take the product of two containers and select the right value */
    @targetName("productR") def :*> (fb: => F[B]): F[B] = product(fb).map(_._2)
    
    /** productL - take the product of two containers and select the left value */
    @targetName("productL") def <*: (fb: => F[B]): F[A] = product(fb).map(_._1)
  
  extension [A] (as: List[F[A]])
    /** sequence - invert a list of containers with individual values into a single container with a list of values. */
    def sequence: F[List[A]] = as.traverse(identity)
  
  extension [A, B](as: List[A])
    /** traverse - apply the function returning a container to each element in the list
     *             and return a new container with the list of results inside. */
    def traverse(f: => A => F[B]): F[List[B]] =
      as.foldRight(pure(List.empty[B])) { 
        (a: A, acc: F[List[B]]) =>
          f(a) <*> (acc map { bs => b => (b +: bs) })
      }