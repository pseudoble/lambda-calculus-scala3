package sandbox 

import parser._
import parser.BasicParsers._
import lambda._
import helpers._

import scala.util.chaining.scalaUtilChainingOps

object tryOuts {

  @main def tryOperators  =   {
    def runAndPrint[A](p: Parser[A]) = p.runParser("charles") tap println

    val mapper = (b: Int) => (a: Char) => b.toHexString.head
    val flatMapper = (b: Int) => (a: Char) => Parser.pure(b.toHexString.head)

    val subject = charP('c')

    subject.map(c => c.toUpper) tap runAndPrint

    (subject >> mapper(1)) tap runAndPrint    
    (subject >>= flatMapper(1)) tap runAndPrint
  }


  /////////////////////////////////
  // Execute the provided parser and print results to console
  def parseAndPrint[T](p: Parser[T])(label: String): ParserFunc[T] = s => p.runParser(s) tap {
    case Some((r, tree)) => println(s"[$r] $label: $s => $tree")
    case None => println("Parsing failed")
  }
  val parseExpr = parseAndPrint(exprP)

  import LcExpr._
  def parens(s: String): String = s"($s)"

  def resolveBindings(expr: LcExpr, env: Map[String, Int]): LcExpr = expr match {
    case LcVar(name) => LcVar(name + env.getOrElse(name, "#"))
    case LcFunc(lcVar @ LcVar(name), body: LcExpr) =>
      val env2 = env + (name -> (env.getOrElse(name, 0) + 1))
      val v = resolveBindings(lcVar, env2) match {
        case x: LcVar => x
        case _ => throw new Exception("shouldn't happen")
      }
      val b = resolveBindings(body, env2)
      LcFunc(v, b)
    case LcApp(left, right) =>
      val l = resolveBindings(left, env)
      val r = resolveBindings(right, env)
      LcApp(l,r)
  }

  def walk(expr: LcExpr): String = expr match {
    case LcVar(name) => name

    case LcFunc(lcVar @ LcVar(name), body: LcExpr) =>
      val v = walk(lcVar)
      val b = walk(body)
      body match {
        case _: LcFunc =>
          val b2 = b.slice(1, b.length)
          s"λ$v$b2"
        case _ => s"λ$v.$b"
      }

    case LcApp(left, right) =>
      val lstr = walk(left)
      val rstr = walk(right)

      def walkLeft(e: LcExpr): String = {
        val str = walk(e)
        e match {
          case _: LcFunc => parens(str)
          case _ => str
        }
      }
      def walkRight(e: LcExpr): String = {
        val str = walk(e)
        e match {
          case _: (LcApp|LcFunc) => parens(str)
          case _ => str
        }
      }
      walkLeft(left) + walkRight(right)
  }
  @main def tryParser = {

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
    fancyParse.map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }

    val fancyParse2 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.x(yz)))y")
    fancyParse2.map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }

    val fancyParse3 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.xyz(λz.z)x))y")
    fancyParse3.map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }


    parseExpr("paren test 1 - expected 'abc'")("abc")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 1.a - expected 'abc'")("(ab)c")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 2 - expected 'a(bc)'")("a(bc)")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 3 - expected '(λx.x)y'")("(λx.x)y")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 4 - expected '(λx.x)(yz)'")("(λx.x)(yz)")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 5 - expected '(λx.x)λx.x'")("(λx.x)(λx.x)")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("paren test 6 - expected '(λx.x)λx.x'")("λx.x(λx.x)")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("param test 1 - expected 'λxy.xy'")("λx.λy.xy")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
    parseExpr("param test 1 - expected 'λx.(λy.x)y'")("λx.(λy.x)y")
      .map { case (_, t) => resolveBindings(t, Map()).pipe(walk).tap(println) }
  }
  @main def runValidate = {
    def validate(s: String)(expected: String): Unit = parseExpr(s)(s).map{ x => x match { case (_, t) => walk(t) } }.map(s2 => println(s"$s == $s2 => ${s2==expected}"))
    validate("λx.x")("λx.x")
    validate("(λx.x)y")("(λx.x)y")
    validate("(λx.xy)")("λx.xy")
    validate("(λx.(λy.xy))y")("(λxy.xy)y")
    validate("(λx.(λy.(x(λx.xy))))y")("(λxy.x(λx.xy))y")
    println("done.")
    
  }
}