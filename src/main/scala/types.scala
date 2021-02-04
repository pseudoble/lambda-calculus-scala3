package types

import scala.Function.untupled
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

enum LcExpr:
  case LcVar(name: String)
  case LcFunc(name: LcVar, expression: LcExpr)
  case LcApp(left: LcExpr, right: LcExpr)
end LcExpr 

type ParserResult[T] = (String, T)
type ParserFunc[A] = String => Option[ParserResult[A]]

case class Parser[+A](runParser: ParserFunc[A])

/* Functor extensions */
extension[A,B] (p: Parser[A]) def map(f: A => B): Parser[B] = Parser {
  p.runParser(_).map {
    case (sout, result) => (sout, f(result))
  }
}

/* Monad extensions */
extension[A,B] (p: Parser[A]) def flatMap(f: A => Parser[B]): Parser[B] = Parser { stream =>
  p.runParser(stream).map { case (s,a) => f(a).runParser(s) }.flatten
}

/** Alternative extensions 
 * Implements Alternative composition for Parser. 
 * Compose Parsers 'left' and 'right' such that the execution will try
 * the left parser first. If left fails, the right parser will be tried. */
extension[A] (left: Parser[A]):
  def alt(right: Parser[A]): Parser[A] = Parser { stream =>
    left.runParser(stream) orElse right.runParser(stream)
  }
  def <|>(right: => Parser[A]): Parser[A] = alt(right)
end extension

/** Extend Parsers returning Functions to support Applicative application. 
 *      P[A => B] <*> P[A] => P[B] */
extension[A,B] (left: Parser[A => B]):
  //    Parser(input => for {
  //      (fout, f) <- left.runParser(input)
  //      (aout, a) <- right.runParser(fout)
  //    } yield (aout, f(a)))
  def app(right: Parser[A]): Parser[B] =
    left.flatMap(f => right.map(f))
  def <*>(right: => Parser[A]): Parser[B] = app(right)
end extension

/** Parser flow extensions */
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
end extension

  
object Parser:
  // applicative
  def pure[A](a: A): Parser[A] = Parser { i => Some((i, a)) }
  
  // alternative 
  def empty[A]: Parser[A] = Parser { _ => None }
  
  def many[A](p: Parser[A]): Parser[List[A]] = Parser { stream => 
    @tailrec
    def f(fstream: String, acc: List[A]): Option[ParserResult[List[A]]] = 
      p.runParser(fstream) match {
        case Some((s, v)) => f(s, v +: acc)
        case None => Some((fstream, acc.reverse))
      }
    f(stream, List())
  }
  
  // ???
  def traverseA[A,B](as: List[A])(f: A => Parser[B]): Parser[List[B]] =
    as.reverse.foldRight(Parser.pure(List.empty[B])) { (a: A, acc: Parser[List[B]]) =>
      val pb: Parser[B] = f(a)
      val pf: Parser[B => List[B]] = acc.map { bs => b => (b +: bs) }
      pf <*> pb
    }.map(_.reverse)

  def seqA[A](fa: List[Parser[A]]): Parser[List[A]] = 
    traverseA(fa)(fa => fa.map(List(_))).map(_.flatten)
      
end Parser

// pattern matching a string like a list
object scons {
  def apply(h: Char, t: String): String = h +: t
  def unapply(s: String): Option[(Char, String)] = s.headOption.map{ (_, s.tail) }
}

// parsers

// fails always
//def failP[A]: Parser[A] = Parser { _ => None }

// parsers the desired character 'x' or fails
def charP(x: Char): Parser[Char] = Parser { 
  case y scons ys if y == x => Some((ys, x))
  case nomatch => None
}

// parses the desired string 'x' or fails
def stringP(x: String): Parser[String] = 
  Parser.seqA((x map charP).toList).map(_.mkString)

// reads the stream character by character until the predicate 'f' 
// returns false. Returns the chars that matched the predicate.
def spanP(f: Char => Boolean): Parser[String] = Parser { input =>
  input.takeWhile(f) match {
    case "" => None
    case s => Some((input.substring(s.length), s))
  }
}

// reads a single lambda calculus variable
def lcVarP: Parser[LcExpr.LcVar] = Parser {
  case y scons ys if y.isLetter => Some((ys, LcExpr.LcVar(y.toString)))
  case nomatch => None
}

// reads a lambda calculus expression wrapped in parens
def lcScopedP: Parser[LcExpr] = charP('(') *> lcExprP <* charP(')')

// reads a lambda calculus application expression
def lcAppP: Parser[LcExpr] = 
  Parser.many(lcScopedP <|> lcFuncP <|> lcVarP) map (_.reduceLeft(LcExpr.LcApp(_,_)))

// reads a lambda calculus function
def lcFuncP: Parser[LcExpr] = for {
  vs <- charP('λ') *> Parser.many(lcVarP)
  e <- charP('.') *> lcExprP
} yield vs.foldRight(e)(LcExpr.LcFunc(_, _))

// reads any lambda calculus expression
def lcExprP: Parser[LcExpr] = lcAppP <|> lcScopedP <|> lcFuncP <|>  lcVarP

/////////////////////////////////
// Execute the provided parser and print results to console
def parseAndPrint[T](p: Parser[T])(label: String): ParserFunc[T] = s => p.runParser(s) tap {
  case Some((r, tree)) => println(s"[$r] $label: $s => $tree")
  case None => println("Parsing failed")
}

@main def run = {
  val parseExpr = parseAndPrint(lcExprP)

  parseExpr("simplest expr")("x")
  parseExpr("simplest app")("xy")
  parseExpr("compound app")("xyz")

  val basicFunction = parseExpr("basic function")("λx.x")
  val longForm = parseExpr("full curried")("λx.λy.x")
  val shortForm = parseExpr("short curried")("λxy.x")
  longForm == shortForm tap println

  parseExpr("short curried, app in body")("λxy.xy")
  parseExpr("same as previous, with explicit parens changing order of operations")("(λxy.x)y")
  val fancyParse = parseExpr("book page 3 bottom")("(λx.(λx.λy.xyz))y")
  
  import LcExpr._
  def walk(expr: LcExpr, env: Map[String, Int]): String = expr match {
    case LcFunc(LcVar(name), body: LcExpr) =>
      val env2 = env + (name -> (env.getOrElse(name, 0) + 1))
      val v = s"$name${env2(name)}"
      val b = walk(body, env2)
      body match {
        case _: LcApp => s"λ$v.$b"
        case _ => s"λ$v.($b)"
      }
    case LcApp(left, right) =>
      val l = left match {
        case _: LcVar => walk(left, env)
        case _ => s"(${walk(left, env)})"
      }
      val r = right match {
        case _: LcVar => walk(right, env)
        case _ => s"{${walk(right, env)}}"
      }
      s"$l$r"
    case LcVar(name) =>
      val n = s"$name${ env.get(name).map(_.toString).getOrElse(s"?") }"
      s"$n"
  }

  fancyParse.map{ case (_, t) => walk(t, Map()) tap println } 
  
  println("done.")
}