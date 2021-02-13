package io.pseudoble.lambda

import io.pseudoble.effects._
import io.pseudoble.parser._
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.tools._

import scala.Function.untupled
import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

/** Lambda Calculus AST Expression types */
enum LcExpr:
  /** A Variable Expression */
  case LcVar(name: String)
  /** A Function Expression */
  case LcFunc(param: LcVar, expression: LcExpr)
  /** An Application Expression */
  case LcApp(left: LcExpr, right: LcExpr)

import LcExpr._

/** Parses any single character that isn't in the set [λ. \t()] as variable. */
val validVars = raw"^([^λ. \t()])(.*)".r
def varP: Parser[LcVar] = Parser { 
  case validVars(h,t) => 
    val name = h.toString
    val param: LcVar = LcVar(name)
    Some((t, param))
  case nomatch => None
} 

/** Parses an expression between parens. */
def parensP: Parser[LcExpr] = 
  charP('(') `:*>` exprP <*: charP(')')

/** Parses a series of Appliation expressions, accounting for parens.  */
def appP: Parser[LcApp] = for {
  // first operand must be a var or in parens to disambiguate
  fst <- (parensP <|> varP): Parser[LcExpr]
  
  // second operand can be anything, but funcs should be in parens to disambiguate
  rest <- atLeast(1)(parensP <|> appP <|> varP): Parser[Vector[LcExpr]]
  
  // build the nested sequence of applications (abc => LcApp(LcApp(a,b),c))
  appExpr = {
    val first: LcApp = LcApp(fst, rest.head) // the first two operands are required
    rest.tail match { 
      case IndexedSeq() => first // if there are only two, we are done
      case items => items.foldLeft(first)(LcApp(_,_)) // if there are more operands, fold over them to nest
    }
  }
} yield appExpr

/** Parses a curried function expression (ie λxyz.xzy) */
def funcP: Parser[LcFunc] = { 
  for {
    // first variable must be preceeded by a lambda.  Additional variables may follow as syntactic sugar over nested single-argument (curried) functions
    vars <- charP('λ') :*> atLeast(1)(varP)
    // After the vars, the body follows (separated by a period). It may be anything.
    body <- charP('.') :*> exprP
    funcExpr = {
      val coreFunc: LcFunc = LcFunc(vars.last, body) // We must have at least one function. This will be the inner-most function (last var)
      vars.dropRight(1) match {
        case IndexedSeq() => coreFunc // if there was only one var, we're done
        case items => items.foldRight(coreFunc)(LcFunc(_, _)) // if there are more vars, foldRight on them to create the nested data structure
      }
    }
  } yield funcExpr 
}

/** Parses anything */
def exprP: Parser[LcExpr] = appP <|> parensP <|> funcP <|> varP


//var i: Long = 0

///** Walk the tree and assign a fingerprint to every unique variable declaration and all the places it's used. 
// *  Variables found without a declaration in scope get no fingerprint. */
//def fingerprint(expression: LcExpr): LcExpr = {
//  type ScopeMap = Map[String, LcVar]
//  type ResolveState[A] = State[ScopeMap, A]
//
//  def makeFingerprint(param: LcVar): LcVar = {
//    i+=1
//    LcVar(param.name, Some(i))
//  }
//
//  def handleFunc(f: LcFunc): ResolveState[LcExpr] = State { env =>
//    val param = makeFingerprint(f.param)
//    val scopedEnv = env + (f.param.name -> param)
//    val resolvedBody = _fp(f.expression).runV(scopedEnv)
//    val func = LcFunc(param, resolvedBody)
//    (env, func)
//  }  
//  
//  def handleVar(v: LcVar): ResolveState[LcExpr] = State { env => 
//    (env, env.getOrElse(v.name, v))
//  }
//  
//  def handleApp(a: LcApp): ResolveState[LcExpr] = for {
//    l <- _fp(a.left)
//    r <- _fp(a.right)
//  } yield LcApp(l, r) 
//  
//  def _fp(expr: LcExpr): ResolveState[LcExpr] = expr match {
//    case v: LcVar => handleVar(v)
//    case a: LcApp => handleApp(a)
//    case f: LcFunc => handleFunc(f)
//  }
//  
//  _fp(expression).runV(Map())
//}

//val VARNAMES = ('a' to 'z').toSet



def replace(v: LcVar, valueFac: () => LcExpr)(target: LcExpr): LcExpr = {
  val _replaceIn = replace(v, valueFac) _
  target match {
    case `v` => valueFac()
    case LcApp(l,r) => LcApp(_replaceIn(l),_replaceIn(r))
    case LcFunc(arg, body) => LcFunc(arg, _replaceIn(body))
    case other => other
  }
}

def betaReduce(expr: LcExpr): LcExpr = expr match {
  case f@LcFunc(param, body) =>
    LcFunc(param, betaReduce(body))
  case LcApp(replaceIn, replaceWith) => {
    val inReduced = betaReduce(replaceIn)
    val withReduced = betaReduce(replaceWith)
    inReduced match {
      case f@LcFunc(param, body) =>
        val replaced = replace(param, () => withReduced)(body)
        betaReduce(replaced)
      case in => LcApp(in, withReduced)
    }
  }
  case o => o
}

def referenced(param: LcVar)(expr: LcExpr): Boolean = expr match {
  case `param` => true
  case LcFunc(_, body) => referenced(param)(body)
  case LcApp(left, right) => referenced(param)(left) || referenced(param)(right)
  case _ => false
}
def etaConvertReduce(expr: LcExpr): LcExpr = expr match {
  case func@LcFunc(param, body) =>
    etaConvertReduce(body) match {
      case LcApp(left, `param`) =>
        if referenced(param)(left)
        then func // func cannot be simplified
        else left // func can be simplified to just left
      case _ => func  // func cannot be simplified
    }
  case o => o // o cannot be simplified
}


/** Walk the tree and build a string representation of it. This will output syntactic sugar for curried functions
 * and apply parens appropriately! */
def asStr(expr: LcExpr): String = {
  def parens(s: String): String = s"($s)"
  
  expr match {
    case LcVar(name) => name
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
      val l = left match {
        case e: LcFunc => parens(asStr(e))
        case e => asStr(e)
      }
      val r = right match {
        case e: (LcApp | LcFunc) => parens(asStr(e))
        case e => asStr(e)
      }
      l + r
  }
}

def printPretty = asStr andThen println