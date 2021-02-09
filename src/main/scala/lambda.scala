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
  case LcVar(name: String, id: Option[Long] = None)
  /** A Function Expression */
  case LcFunc(param: LcVar, expression: LcExpr)
  /** An Application Expression */
  case LcApp(left: LcExpr, right: LcExpr)

import LcExpr._

/** Parses a single letter variable. */
def varP: Parser[LcVar] = Parser {
  case h scons t if h.isLetter => 
    val name = h.toString
    val param: LcVar = LcVar(name, None)
    Some((t, param))
  case nomatch => None
} 

/** Parses an expression between parens. */
def parensP: Parser[LcExpr] = charP('(') :*> exprP <*: charP(')')

/** Parses an Appliation expression. */
def appP: Parser[LcExpr] = 
  many(parensP <|> funcP <|> varP) map (_.reduceLeft(LcApp(_,_)))

/** Parses a function expression */
def funcP: Parser[LcExpr] =
  ((charP('λ') :*> many(varP)) * (charP('.') :*> exprP))
    .map((vars,body) => vars.foldRight(body)(LcFunc(_, _)))

/** Parses anything */
def exprP: Parser[LcExpr] = appP <|> parensP <|> funcP <|> varP


var i: Long = 0

/** Walk the tree and assign a fingerprint to every unique variable declaration and all the places it's used. 
 *  Variables found without a declaration in scope get no fingerprint. */
def fingerprint(expression: LcExpr): LcExpr = {
  type ScopeMap = Map[String, LcVar]
  type ResolveState[A] = State[ScopeMap, A]

  def makeFingerprint(param: LcVar): LcVar = {
    i+=1
    LcVar(param.name, Some(i))
  }

  def handleFunc(f: LcFunc): ResolveState[LcExpr] = State { env =>
    val param = makeFingerprint(f.param)
    val scopedEnv = env + (f.param.name -> param)
    val resolvedBody = _fp(f.expression).runV(scopedEnv)
    val func = LcFunc(param, resolvedBody)
    (env, func)
  }  
  
  def handleVar(v: LcVar): ResolveState[LcExpr] = State { env => 
    (env, env.getOrElse(v.name, v))
  }
  
  def handleApp(a: LcApp): ResolveState[LcExpr] = for {
    l <- _fp(a.left)
    r <- _fp(a.right)
  } yield LcApp(l, r) 
  
  def _fp(expr: LcExpr): ResolveState[LcExpr] = expr match {
    case v: LcVar => handleVar(v)
    case a: LcApp => handleApp(a)
    case f: LcFunc => handleFunc(f)
  }
  
  _fp(expression).runV(Map())
}

def replace(v: LcVar, valueFac: () => LcExpr)(target: LcExpr): LcExpr = {
  val _replaceIn = replace(v, valueFac) _
  target match {
    case `v` => valueFac()
    case LcApp(l,r) => LcApp(_replaceIn(l),_replaceIn(r))
    case LcFunc(arg, body) => LcFunc(arg, _replaceIn(body))
    case other => other
  }
}

def betaReduce(expr: LcExpr): LcExpr = {
  (expr match {
    case LcApp(replaceIn, replaceWith) => {
      val inReduced = betaReduce(replaceIn)
      val withReduced = betaReduce(replaceWith)
      inReduced match {
        case LcFunc(param, body) =>
          replace(param, () => fingerprint(withReduced))(body)
          |> betaReduce
        case in => LcApp(in, withReduced)
      }
    }
    case o => o
  })
}


/** Walk the tree and build a string representation of it. This will output syntactic sugar for curried functions
 * and apply parens appropriately! */
def asStr(expr: LcExpr): String = {
  def parens(s: String): String = s"($s)"
  
  expr match {
    case LcVar(name, _) => name
    //  case LcVar(name, id) => id match {
    //    case Some(i) => s"$name#$i"
    //    case _ => name
    //  }
    case LcFunc(lcVar, body) =>
      val v = asStr(lcVar)
      val b = asStr(body)
      body match {
        case _: LcFunc =>
          val b2 = b.slice(1, b.length)
          s"λ$v$b2"
        case _ => s"λ$v.$b"
      }

    case LcApp(left, right) =>
      def w(e: LcExpr): String = {
        val str = asStr(e)
        e match {
          case _: (LcApp|LcFunc) => parens(str)
          case _ => str
        }
      }
      w(left) + w(right)
  }
}

def printPretty = asStr andThen println