package effects

import scala.util.chaining.scalaUtilChainingOps
import helpers._

/** Defines a Functor and related convenience functions */
trait Functor[F[_]]:
  /** Apply the function 'f' to the value in the container and return a new container with the new value */
  def fmap[A,B](fa: F[A])(f: A => B): F[B]

  /** Transform the function 'f' from A => B to F[A] => F[B] */
  def lift[A,B](f: A => B): F[A] => F[B] = fa => fmap(fa)(f)
  
  /** Apply the function 'f' to the value in the container and return a new container with a tuple containing the original value and the new value */
  def product[A,B](fa: F[A])(f: A => B): F[(A, B)] = fmap(fa)(a => (a, f(a)))


  /** Apply the function 'f' to the value in the container and return a new container with the new value */
  extension [A, B] (fa: F[A]) def map(f: A => B): F[B] = fmap(fa)(f)
  /** Map operator. */
  extension [A, B] (fa: F[A]) def >> (f: A => B): F[B] = fmap(fa)(f)

trait Apply[F[_]] extends Functor[F]:
  /** Apply a function in one container to the value in another container and return a new container with the new value */
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  /** Apply operator. */
  extension [A, B](fa: F[A]) def <*>(f: F[A => B]): F[B] = ap(fa)(f)

trait Applicative[F[_]] extends Apply[F]:
  def pure[A](x: A): F[A]
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.reverse.foldRight(pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
      ap(f(a))(fmap(acc){ bs => b => (b +: bs) })
    } |> (fmap(_)(_.reverse))
  extension [A] (as: List[F[A]]) def sequence: F[List[A]] = traverse(as)(identity)

trait Alternative[F[_]] extends Applicative[F]:
  def empty[A]: F[A]
  def alt[A](fa: F[A])(other: F[A]): F[A]
  extension [A] (fa: F[A]) def orElse (other: F[A]): F[A] = alt(fa)(other)
  /** Alternative operator. */
  extension [A] (fa: F[A]) def <|> (other: F[A]): F[A] = alt(fa)(other)

trait Monad[F[_]] extends Applicative[F]:
  def mflatten[A](ffa: F[F[A]]): F[A]
  def mflatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = mflatten(fmap(fa)(f))
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = mflatMap(fa)(a => fmap(f)(ff => ff(a)))
  extension [A] (ffa: F[F[A]]) def flatten: F[A] = mflatten(ffa)
  extension [A, B] (fa: F[A]) def flatMap(f: A => F[B]): F[B] = mflatMap(fa)(f)
  extension [A, B] (fa: F[A]) def >>=(f: A => F[B]): F[B] = mflatMap(fa)(f)


