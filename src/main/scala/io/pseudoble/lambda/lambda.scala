package io.pseudoble.lambda

import io.pseudoble.effects.instances.State

import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex
import io.pseudoble.lambda.types._
import io.pseudoble.tools._

import EnvStack._

case class EnvStackFrame(binding: Expr.Binding, vars: Set[VarTerm])
case class EnvStack(frames: List[EnvStackFrame])

object EnvStack {
  import Expr._
  type VarTerm = FreeVar | BindingRef

  def addFrame(stack: EnvStack)(binding: Binding): EnvStack =
    EnvStack(EnvStackFrame(binding, Set()) +: stack.frames)

  def dropFrame(stack: EnvStack): (Set[VarTerm], EnvStack) = {
    val (fst +: rem) = stack.frames
    // get the FreeVars to merge down
    val freeVars: Set[VarTerm] = fst.vars.flatMap {
      // current function's bindings don't merge down
      case BindingRef(0) => Set()
      // BindingRefs with idx > 0, though, do.
      // Merging down requires their indexes be decremented
      case BindingRef(n) => Set { BindingRef(n - 1) }
      // FreeVars are always free in this setup
      case other => Set { other }
    }
    val newStack = rem match {
      case snd +: t =>
        // and merge them down to the new stack head
        val newHead = EnvStackFrame (snd.binding, snd.vars ++ freeVars)
        EnvStack(newHead +: t)
      case _ => EnvStack(Nil)
    }

    (freeVars, newStack)
  }

  def addVar(stack: EnvStack)(v: VarTerm): EnvStack = stack.frames match {
    case h +: t => EnvStack(EnvStackFrame(h.binding, h.vars + v)  +: t)
    case other => throw Exception("bad juju 1!")
  }

  def mergeStacks(left: EnvStack, right: EnvStack): EnvStack = (left.frames, right.frames) match {
    case (lh +: lt, rh +: rt) if lt == rt => EnvStack(mergeFrames(lh, rh) +: lt)
    case other => throw Exception("bad juju 2!")
  }

  def mergeFrames(left: EnvStackFrame, right: EnvStackFrame): EnvStackFrame = (left, right) match {
    case (EnvStackFrame(lb, lv), EnvStackFrame(rb, rv)) if lb == rb => EnvStackFrame(lb, lv ++ rv)
    case other => throw Exception("bad juju 3!")
  }
}

def betaReduce(expr: Expr): Expr = {
  import Expr._

  def renameBinding(func: Func, stack: EnvStack): Func = {
    val takenNames = Set.from(stack.frames.map(_.binding.name))
    def tickName(name: String): String = {
      val candidate = name + "'"
      if !takenNames.contains(candidate)
      then candidate
      else tickName(candidate)
    }
    Func(Binding(tickName(func.binding.name)): Binding, func.expression)  
  }
  
  def hasBindingConflict(func: Func, bodyFreeVars: Set[VarTerm], stack: EnvStack): Boolean = bodyFreeVars.exists {
    case FreeVar(name) if name == func.binding.name => true
    case BindingRef(n) => stack.frames.length > n && stack.frames(n).binding.name == func.binding.name
    case _ => false
  }
  
  // if this function's binding name exists in the free variables of its children,
  // either as an unbound FreeVar or as a BindingRef to a binding outside this function,
  // then we need to rename this function's binding.
  def alphaConvert(func: Func, bodyFreeVars: Set[VarTerm], stack: EnvStack): Func =     
    if hasBindingConflict(func, bodyFreeVars, stack) 
    then renameBinding(func, stack)
    else func
  
  
  def _reduce(current: Expr, stack: EnvStack): (EnvStack, Expr) = current match {
    case Func(a @ Binding(bindingName), b) =>
      val (myStack, newBody) = _reduce(b, EnvStack.addFrame(stack)(a))
      val (freeVars, outStack) = EnvStack.dropFrame(myStack)
      (outStack, alphaConvert(Func(a, newBody), freeVars, outStack))
    case App(a, b) => 
      val (leftStack, left) = _reduce(a, stack)
      val (rightStack, right) = _reduce(b, stack)
      (left, right) match {
        case (Func(arg, body), appRight) =>
          substitute(0, body, appRight) |> { _reduce(_, stack) }
        case (l, r) =>
          EnvStack.mergeStacks(leftStack, rightStack) |> { (_, App(l, r)) }
      }
      
    case b@BindingRef(n) =>
      val outStack = EnvStack.addVar(stack)(b)
      (outStack, b)
    case v@FreeVar(name) =>
      val outStack = EnvStack.addVar(stack)(v)
      (outStack, v)
    case o =>
      (stack, o)
  }
  
  _reduce(expr, EnvStack(List(EnvStackFrame(Binding("external"), Set()))))._2
}

def etaReduce(expr: Expr): Expr = {
  import Expr._

  expr match {
    case Func(_, App(f: Func, BindingRef(0))) => mapFreeRefs(f)(_ - 1)
    case Func(a, b) => Func(a, etaReduce(b))
    case App(l, r) => App(etaReduce(l), etaReduce(r))
    case o => o
  }
}


// e1 [x := e2]
// replace every occurance of x in e1 with e2
def substitute(xIdx: Int, e1: Expr, e2: Expr): Expr = {
  import Expr._

  def _sub(currE1: Expr, xDelta: Int): Expr = currE1 match {
    case Func(a, b) => Func(a, _sub(b, xDelta + 1))
    case App(left, right) => App(_sub(left, xDelta), _sub(right, xDelta))
    case BindingRef(`xDelta`) => mapFreeRefs(e2)(_ + xDelta)
    case BindingRef(idx) if idx > xDelta => BindingRef(idx - 1)
    case b: BindingRef => b
    case o => o
  }
  _sub(e1, xIdx)
}

def mapFreeRefs(expr: Expr)(f: Int => Int): Expr = {
  import Expr._

  def _map(current: Expr, depth: Int): Expr = current match {
    case Func(a, b) => Func(a, _map(b, depth + 1))
    case App(left, right) => App(_map(left, depth), _map(right, depth))
    case BindingRef(idx) if idx >= depth => BindingRef(f(idx))
    case o => o
  }
  _map(expr, 0)
}

///** Walk the tree and build a string representation of it. This will output syntactic sugar for curried functions
// * and apply parens appropriately! */
def asStr(expr: Expr | LcParser.ParsedExpr): String = {
  def parens(s: String): String = s"($s)"
  val funcRx = "^λ([^.]+)\\.(.*)$".r
  expr match {
    case Expr.Func(arg, body) =>
      val argStr = asStr(arg)
      val bodyStr = asStr(body)
      body match {
        case b: Expr.App => parens { s"λ$argStr.$bodyStr" }
        case b: Expr.Func =>
          bodyStr match {
            case funcRx(ia, ib) => s"λ$argStr $ia.$ib"
            case other => s"λ$argStr.$other"
          }
        case b => s"λ$argStr.$bodyStr"
      }
    case LcParser.ParsedExpr.Func(arg, body) =>
      val argStr = asStr(arg)
      val bodyStr = asStr(body)
      body match {
        case b: LcParser.ParsedExpr.App => parens { s"λ$argStr.$bodyStr" }
        case b: LcParser.ParsedExpr.Func =>
          bodyStr match {
            case funcRx(ia, ib) => s"λ$argStr $ia.$ib"
            case other => s"λ$argStr.$other"
          }
        case b => s"λ$argStr.$bodyStr"
      }

    case Expr.App(left, right) =>
      val l = left match {
        case e: Expr.Func => parens(asStr(e))
        case e => asStr(e)
      }
      val r = right match {
        case e: (Expr.App | Expr.Func) => parens(asStr(e))
        case e => asStr(e)
      }
      s"$l $r"
    case LcParser.ParsedExpr.App(left, right) =>
      val l = left match {
        case e: LcParser.ParsedExpr.Func => parens(asStr(e))
        case e => asStr(e)
      }
      val r = right match {
        case e: (Expr.App | Expr.Func) => parens(asStr(e))
        case e => asStr(e)
      }
      s"$l $r"
      
    case LcParser.ParsedExpr.Literal(value) => value.toString

    case Expr.Binding(name) => name
    case LcParser.ParsedExpr.Var(name) => name
      
    case Expr.BindingRef(idx) => s"<$idx>"
      
    case Expr.FreeVar(name) => s"$name"
  }
}

def toIdx(expr: LcParser.ParsedExpr): Expr = {
  def _toIdx(current: LcParser.ParsedExpr, indices: List[String]): Expr = current match {
    case LcParser.ParsedExpr.Func(LcParser.ParsedExpr.Var(name), b) => Expr.Func(Expr.Binding(name), _toIdx(b, name +: indices))
    case LcParser.ParsedExpr.App(a, b) => Expr.App(_toIdx(a, indices), _toIdx(b, indices))
    case LcParser.ParsedExpr.Var(name) => indices.indexOf(name) match {
      case -1 => Expr.FreeVar(name)
      case n => Expr.BindingRef(n)
    }
  }
  _toIdx(expr, List())
}

def fromIdx(expr: Expr): LcParser.ParsedExpr = {
  import LcParser.{ ParsedExpr => PExpr }
  
  def _fromIdx(current: Expr, indices: List[String]): PExpr = 
    current match {
      case Expr.Func(a @ Expr.Binding(name), b) => PExpr.Func(
        { 
          val x: PExpr = _fromIdx(a, indices)
          x.asInstanceOf[PExpr.Var]
        },
        _fromIdx(b, name +: indices))
      case Expr.App(a, b) => 
        PExpr.App(_fromIdx(a, indices), _fromIdx(b, indices))
      case b@Expr.BindingRef(n) =>
        PExpr.Var(indices(n))
      case v@Expr.FreeVar(name) => PExpr.Var(name)
      case Expr.Binding(name) => PExpr.Var(name)
    }
  _fromIdx(expr, List())    
}