package io.pseudoble.parser

import io.pseudoble.effects.typeclasses._
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

object Parser:
  /** Monad & Alternative implementations for Parser */
  given Monad[Parser] with Alternative[Parser] with {
    def empty[A]: Parser[A] = Parser { _ => None }
    def pure[A](a: => A): Parser[A] = Parser { Some(_, a) }
    extension [A](fa: Parser[A])
      def orElse(other: => Parser[A]): Parser[A] =
        Parser { input =>
          fa.parse(input).orElse(other.parse(input))
        }
    extension [A, B](fa: Parser[A])
      def flatMap(f: => A => Parser[B]): Parser[B] =
        Parser { input =>
          fa.parse(input) match {
            case Some((i, a)) => f(a).parse(i)
            case None => None
          }
        }
  }

