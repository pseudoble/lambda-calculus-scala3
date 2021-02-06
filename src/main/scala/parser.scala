package parser

import effects._
import helpers._
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

/** The successful result of running a parser, containing the remainder of the input string paired with the parsed value */
type ParserResult[T] = (String, T)

/** The function signature of a Parser. String goes in, possible result comes out */
type ParserFunc[A] = String => Option[ParserResult[A]]

/** Class definition of a Parser[A] */
class Parser[+A](private val f: ParserFunc[A]):
  def parse(input: String): Option[ParserResult[A]] = f(input)

object Parser:
  
  /** Monad & Alternative implementations for Parser */
  given Monad[Parser], Alternative[Parser] with {
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
  def charP(x: Char): Parser[Char] = Parser {
    case y scons ys if y == x => Some((ys, x))
    case nomatch => None
  }

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

  /** Returns Parser that executes provided parser repeatedly until it fails. */
  def many[A](p: Parser[A]): Parser[List[A]] = Parser { input =>
    @tailrec
    def f(in: String, acc: List[A]): Option[ParserResult[List[A]]] =
      p.parse(in) match {
        case Some((s, v)) => f(s, v +: acc)
        case None => Some((in, acc.reverse))
      }
    f(input, List())
  }
}
