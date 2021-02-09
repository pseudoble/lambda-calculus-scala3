package sandbox 

import parser._
import effects._
import parser.BasicParsers._
import lambda._
import lambda.LcExpr._
import helpers._

import scala.util.chaining.scalaUtilChainingOps

object tryOuts {

  @main def tryOperators  =   {
    def runAndPrint[A](p: Parser[A]) = p.parse("charles") tap println

    def mapper(b: Int)(a: Char) = b.toHexString.head
    def flatMapper(b: Int)(a: Char)(using parserAp: Applicative[Parser]) = parserAp.pure(b.toHexString.head)

    val subject = charP('c')

    subject.map(c => c.toUpper) tap runAndPrint

    (subject map mapper(1)) tap runAndPrint    
    (subject >>= flatMapper(1)) tap runAndPrint

    val parserAp: Monad[Parser] = summon[Monad[Parser]]

    val f: String => Option[Int] = _.toIntOption
    val input = "42"
    val inputParser = stringP(input)

    def parse42asIntOption_manual: Parser[Option[Int]] = Parser { input =>
      inputParser.parse(input) match {
        case Some((s, value)) => Some((s, f(value)))
        case None => None
      }
    }

    val liftedF = parserAp.lift(f)
    val parse42asIntOption_lifted = liftedF(inputParser)

    val pureF = parserAp.pure(f)
    val parse42asIntOption_pure = (inputParser <*> pureF)

    parse42asIntOption_manual.parse(input) tap println
    parse42asIntOption_lifted.parse(input) tap println
    parse42asIntOption_pure.parse(input) tap println

    { charP('c') :*> charP('h') }.parse("charles") tap println
    { charP('c') <*: charP('h') }.parse("charles") tap println

    val pVars = charP('λ') :*> many(varP)
    val pDotExpr = charP('.') :*> exprP
    val pFunc = pVars * pDotExpr
    val pFunc2 = (charP('λ') :*> many(varP)) * (charP('.') :*> exprP)

    pVars.parse("abc") tap (r => println(s"$r should be None"))
    pVars.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some((".xyz",List(LcVar("a"), LcVar("b"), LcVar("c")))) }"))
    pDotExpr.parse("λabc.xyz") tap (r => println(s"$r should be None"))
    pDotExpr.parse(".xyz") tap (r => println(s"$r => ${ r == Some(("",LcApp(LcApp(LcVar("x"), LcVar("y")), LcVar("z")))) }"))
    pFunc.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("",LcApp(LcApp(LcVar("x"), LcVar("y")), LcVar("z")))) }"))
    pFunc2.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("",LcApp(LcApp(LcVar("x"), LcVar("y")), LcVar("z")))) }"))

    pFunc2
      .map{ (l,r) => l.foldRight(r)(LcFunc(_, _)) }
      .parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("", LcFunc(LcVar("a"), LcFunc(LcVar("b"), LcFunc(LcVar("c"), LcApp(LcApp(LcVar("x"), LcVar("y")), LcVar("z"))))))) }"))
  }

  def parseAndPrint[T](p: Parser[T])(label: String): ParserFunc[T] = (s: String) => p.parse(s) tap {
    case Some((r, tree)) => println(s"[$r] $label: $s => $tree")
    case None => println("Parsing failed")
  }
  val parseExpr = parseAndPrint(exprP)


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


    def thing(r: ParserResult[LcExpr]): LcExpr = r match { case (_, t) => fingerprint(t).tap(asStr(_).tap(println)) }
    val fancyParse = parseExpr("book page 3 bottom")("(λx.(λx.λy.xyz))y")
    fancyParse.map(thing)

    val fancyParse2 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.x(yz)))y")
    fancyParse2.map(thing)

    val fancyParse3 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.xyz(λz.z)x))y")
    fancyParse3.map(thing)


    parseExpr("paren test 1 - expected 'abc'")("abc")
      .map(thing)
    parseExpr("paren test 1.a - expected 'abc'")("(ab)c")
      .map(thing)
    parseExpr("paren test 2 - expected 'a(bc)'")("a(bc)")
      .map(thing)
    parseExpr("paren test 3 - expected '(λx.x)y'")("(λx.x)y")
      .map(thing)
    parseExpr("paren test 4 - expected '(λx.x)(yz)'")("(λx.x)(yz)")
      .map(thing)
    parseExpr("paren test 5 - expected '(λx.x)λx.x'")("(λx.x)(λx.x)")
      .map(thing)
    parseExpr("paren test 6 - expected '(λx.x)λx.x'")("λx.x(λx.x)")
      .map(thing)
    parseExpr("param test 1 - expected 'λxy.xy'")("λx.λy.xy")
      .map(thing)
    parseExpr("param test 1 - expected 'λx.(λy.x)y'")("λx.(λy.x)y")
      .map(thing)
    parseExpr("param test 1 - expected '(λx.xx)(λx.x)'")("(λx.xx)(λx.x)")
      .map(thing)

    println("---------------------------------------")
    
    
    def parseAndReduce(label: String)(expr: String): Option[LcExpr] = {
      val parsed = exprP.parse(expr).map{ case (_, t) => fingerprint(t) }
      val reduced = parsed.map(betaReduce)
      reduced
    }
    parseAndReduce("beta reduce 1")("(λx.x)y") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 2")("(λx.xxx)(λy.y)") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 3")("(λxy.xy)(λy.y)") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 4")("(λxyz.xyz)(λxy.xy)(λx.x)x") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 5")("(λxyz.xyz)(λyx.xzy)(λx.x)x") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 6")("xz(λx.x)") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 7")("λx.yx") tap { _.map(asStr).tap(println) }
    parseAndReduce("beta reduce 8")("(λx.(λy.x)y)z") tap { _.map(asStr).tap(println) }
    
  }
  
  @main def runValidate = {
    def validate(s: String)(expected: String): Unit = parseExpr(s)(s).map{ x => x match { case (_, t) => asStr(t) } }.map(s2 => println(s"$s == $s2 => ${s2==expected}"))
    validate("λx.x")("λx.x")
    validate("(λx.x)y")("(λx.x)y")
    validate("(λx.xy)")("λx.xy")
    validate("(λx.(λy.xy))y")("(λxy.xy)y")
    validate("(λx.(λy.(x(λx.xy))))y")("(λxy.x(λx.xy))y")
    validate("(λx.xx)(λx.x)")("(λx.xx)(λx.x)")
    println("done.")
  }
}