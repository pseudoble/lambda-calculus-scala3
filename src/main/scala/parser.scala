package parser

import effects._
import helpers.scons

import scala.annotation.tailrec

type ParserResult[T] = (String, T)
type ParserFunc[A] = String => Option[ParserResult[A]]

case class Parser[+A](runParser: ParserFunc[A])

object Parser extends ParserEffects:
  
  /** Returns Parser that executes provided parser repeatedly until it fails. */
  def many[A](p: Parser[A]): Parser[List[A]] = Parser { stream =>
    @tailrec
    def f(fstream: String, acc: List[A]): Option[ParserResult[List[A]]] =
      p.runParser(fstream) match {
        case Some((s, v)) => f(s, v +: acc)
        case None => Some((fstream, acc.reverse))
      }
    f(stream, List())
  }

  /** Parser one sided combinators */
  extension[A,B] (left: Parser[A]):
    /* Execute both left and right. If both succeed, discard the left and return the right */
    def *>(right: => Parser[B]): Parser[B] = Parser { stream =>
      for {
        (stream, _) <- left.runParser(stream)
        (stream, rightValue) <- right.runParser(stream)
      } yield (stream, rightValue)
    }
    /* Execute both left and right. If both succeed, discard the right and return the left */
    def <*(right: => Parser[B]): Parser[A] = Parser { stream =>
      for {
        (stream, leftValue) <- left.runParser(stream)
        (stream, _) <- right.runParser(stream)
      } yield (stream, leftValue)
    }

// Effectful implementations for Parser (monads, functors, etc)
trait ParserEffects extends Monad[Parser] with Alternative[Parser]:
  def pure[A](a: A): Parser[A] = Parser { Some(_, a) }
  def fmap[A, B](fa: Parser[A])(f: A => B): Parser[B] = Parser { stream =>
    fa.runParser(stream).map {
      case (sout, result) => (sout, f(result))
    }
  }
  def mflatten[A](ffa: Parser[Parser[A]]): Parser[A] = Parser { stream =>
    ffa.runParser(stream) match {
      case Some((s,fa)) => fa.runParser(s)
      case None => None
    }
  }
  def empty[A]: Parser[A] = Parser { _ => None }
  def alt[A](fa: Parser[A])(alt: Parser[A]): Parser[A] = Parser { stream =>
    fa.runParser(stream).orElse(alt.runParser(stream))
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
}