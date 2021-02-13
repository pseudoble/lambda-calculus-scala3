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

object BasicParsers {

  /** fails always */
  def failP[A]: Parser[A] = Parser { _ => None }

  /** Parses the configured character 'x'. */
  def charP(x: Char): Parser[Char] = Parser { stream => stream match {
    case y scons ys if y == x => Some((ys, x))
    case nomatch => None
  }}

  /** Parses the configured string 'x'. */
  def stringP(x: String): Parser[String] =
    (x map charP).toList.sequence.map(_.mkString)

  /** Parses a sequence of characters matching the configured predicate. */
  def spanP(predicate: Char => Boolean): Parser[String] = Parser { input =>
    input.takeWhile(predicate) match {
      case "" => None
      case s => Some((input.substring(s.length), s))
    }
  }
  
  /** Parses a sequence of characters as whitespace. */
  def wsP: Parser[String] = spanP(_.isWhitespace)

  /** Returns Parser that executes provided parser repeatedly until it fails. */
  def many[A](p: Parser[A]): Parser[Vector[A]] = Parser { input =>
    @tailrec
    def f(in: String, acc: Vector[A]): Option[ParserResult[Vector[A]]] =
      p.parse(in) match {
        case Some((s, v)) => f(s, acc :+ v) 
        case None => Some((in, acc))
      }
    f(input, Vector())
  }
  
  def atLeast[A](count: Int)(p: Parser[A])(using app: Applicative[Parser]): Parser[Vector[A]] =
    many(p) flatMap { (items: Vector[A]) => if items.length >= count then app.pure(items) else failP[Vector[A]] }
  
  def opt[A](p: Parser[A]): Parser[Option[A]] = Parser { stream =>
    p.parse(stream) match {
      case None => Some((stream, None))
      case Some((s,v)) => Some((s,Some(v)))
    }
  }
}
