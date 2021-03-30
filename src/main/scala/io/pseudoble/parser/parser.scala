package io.pseudoble.parser

import io.pseudoble.effects.typeclasses._
import io.pseudoble.effects.typeclasses.Extensions._
import io.pseudoble.tools._

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps
import scala.collection.immutable

/** The successful result of running a parser, containing the remainder of the input string paired with the parsed value */
type ParserResult[T] = (String, T)

/** The function signature of a Parser. String goes in, possible result comes out */
type ParserFunc[A] = String => Option[ParserResult[A]]

/** Class definition of a Parser[A] */
case class Parser[+A](parse: ParserFunc[A])
object Parser {
  given Monad[Parser] with Alternative[Parser] with {
    // Members declared in io.pseudoble.effects.typeclasses.Alternative
    def alt[A](left: Parser[A])(right: Parser[A]): Parser[A] = 
      Parser { input =>
        left.parse(input).orElse(right.parse(input))
      }

    def empty[A]: Parser[A] = 
      Parser { _ => None }

    // Members declared in io.pseudoble.effects.typeclasses.Applicative
    def pure[A](a: A): Parser[A] = 
      Parser { Some(_, a) }

    // Members declared in io.pseudoble.effects.typeclasses.Monad
    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] = 
      Parser { input =>
        fa.parse(input) match {
          case Some((i, a)) => f(a).parse(i)
          case None => None
        }
      }
  }

  extension [A] (as: List[Parser[A]]) // List[F[_]] => F[List[_]]
    /** sequence - invert a list of containers with individual values into a single container with a list of values. */
    def sequence: Parser[List[A]] = as.traverse(identity)
  
  extension [A](as: List[A])
    /** traverse - apply the function returning a container to each element in the list
     *             and return a new container with the list of results inside. */
    def traverse[B](f: => A => Parser[B])(using applicative: Applicative[Parser]): Parser[List[B]] =
      as.foldLeft(applicative.pure(Vector.empty[B])) { 
        (acc: Parser[Vector[B]], a: A) =>
          acc map { 
            (bs: Vector[B]) => (b: B) => bs :+ b            
          } pipe (_ <*> f(a))
      }.map(_.toList)

}
