package io.pseudoble.parser

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.chaining.scalaUtilChainingOps

import io.pseudoble.tools._
import io.pseudoble.effects.typeclasses._
import io.pseudoble.effects.typeclasses.Extensions._

object BasicParsers {
  private def pure[A](a: A)(using app: Applicative[Parser]): Parser[A] = app.pure(a)

  /** fails always */
  def failP[A]: Parser[A] = Parser { _ => None }
  
  /** succeed with provided value, taking nothing from the stream */
  def constP[A](a: A): Parser[A] = pure(a)

  /** Parses the configured character 'x'. */
  def charP(x: Char): Parser[Char] = Parser { stream =>
    if (stream.isEmpty == false && stream(0) == x)
      Some((stream.substring(1), x))
    else 
      None
  }

  /** Parses the configured string 'x'. */
  def stringP(x: String): Parser[String] =
    (x map charP).toList.sequence.map(_.mkString)

  /** Parses a sequence of characters matching the configured predicate. */
  def spanP(predicate: Char => Boolean): Parser[String] = Parser { stream =>
    stream.takeWhile(predicate) match {
      case "" => None
      case s => Some((stream.substring(s.length), s))
    }
  }
  
  def parsePositiveInt: Parser[Int] = spanP(c => c.isDigit).map(_.toInt)
  
  /** Parses a sequence of characters as whitespace. */
  def wsP: Parser[String] = spanP(_.isWhitespace)

  def regexP(r: Regex): Parser[String] = Parser { stream => 
    r.findFirstIn(stream) map { v => (stream.substring(v.length), v) }
  } 
    
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