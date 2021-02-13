package io.pseudoble.effects

import scala.annotation.targetName
import scala.util.chaining.scalaUtilChainingOps
import io.pseudoble.tools._

/** Functor Type Class */
trait Functor[F[_]]:

  /** lift - given a function A => B, return a new function F[A] => F[B] */
  def lift[A, B](f: => A => B): F[A] => F[B] = a => map(a)(f)

  extension [A, B] (fa: F[A])
    /** map - given a container with a value, apply a function to to the value and return a new container holding the 
     *        result. */
    infix def map(f: => A => B): F[B]
    
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

/** Alternative Type Class */
trait Alternative[F[_]] extends Applicative[F]:
  /** empty - A container that may not have a value must have an empty state. */
  def empty[A]: F[A]
  
  extension [A] (fa: F[A])
    /** alternative / orElse - Try the first container. If it's empty, return the second container instead. */
    infix def orElse(other: => F[A]): F[A]
    @targetName("alternative") def <|>(other: => F[A]) = fa orElse other

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

/** A type instance for the State Monad. */
class State[S, +A](private val f: S => (S, A)) {
  def run(state: S): (S,A) = f(state)
  def runV(state: S): A = f(state)._2
  def runS(state: S): S = f(state)._1
}
object State:
  type StateInstance[S] = [a] =>> State[S, a]
  given [S]: Monad[StateInstance[S]] with {
    def pure[A](a: => A): State[S, A] = State { s => (s, a) }
    extension [A, B](fa: State[S, A])
      def flatMap(f: => A => State[S, B]): State[S, B] =
        State { s =>
          fa.f(s) match {
            case (s2, a) => f(a).f(s2)
          }
        }
  }