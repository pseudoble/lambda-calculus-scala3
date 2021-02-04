package lambda

import effects._
import parser._
import parser.BasicParsers._
import helpers._

import scala.Function.untupled
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

/** Lambda Calculus AST Expression types */
enum LcExpr:
  /** A Variable Expression */
  case LcVar(name: String)
  /** A Function Expression */
  case LcFunc(name: LcVar, expression: LcExpr)
  /** An Application Expression */
  case LcApp(left: LcExpr, right: LcExpr)

import LcExpr._


/** Parses a single letter variable. */
def varP: Parser[LcVar] = Parser {
  case y scons ys if y.isLetter => Some((ys, LcVar(y.toString)))
  case nomatch => None
}

/** Parses an expression between parens. */
def parensP: Parser[LcExpr] = charP('(') *> exprP <* charP(')')

/** Parses an Appliation expression. */
def appP: Parser[LcExpr] = 
  Parser.many(parensP <|> funcP <|> varP) >> (_.reduceLeft(LcApp(_,_)))
  
/** Parses a function expression */
def funcP: Parser[LcExpr] = for {
  vs <- charP('λ') *> Parser.many(varP)
  e <- charP('.') *> exprP
} yield vs.foldRight(e)(LcFunc(_, _))

/** Parses anything */
def exprP: Parser[LcExpr] = appP <|> parensP <|> funcP <|> varP
