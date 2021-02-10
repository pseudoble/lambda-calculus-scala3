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


    def thing(r: ParserResult[LcExpr]): LcExpr = r match { case (_, t) => fingerprint(t).tap(asStr(false)(_).tap(println)) }
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
    
    
    def parseAndReduce(label: String)(input: String)(expected: String): Option[LcExpr] = for {
      (_, parsed) <- exprP.parse(input)
//      _ = printPretty(parsed)
      fingerprinted = fingerprint(parsed)
      betaReduced = betaReduce(fingerprinted)
//      _ = printPretty(betaReduced)
      etaReduced = etaConvertReduce(betaReduced)
//      _ = printPretty(etaReduced)
      success = asStr(false)(etaReduced) == expected
      _ = println(s"[$success] $input -> ${asStr(false)(parsed)} >> ${asStr(false)(betaReduced)} >> ${asStr(false)(etaReduced)} == $expected")
//      _ = println("-------")
    } yield etaReduced
    
    parseAndReduce("beta reduce 1")("(λx.x)y")("y")
    parseAndReduce("beta reduce 2")("(λx.xxx)(λy.y)")("λy.y")
    parseAndReduce("beta reduce 3")("(λxy.xy)(λy.y)")("λy.y")
    parseAndReduce("beta reduce 4")("(λxyz.xyz)(λxy.xy)(λx.x)x")("x")
    parseAndReduce("beta reduce 5")("(λxyz.xyz)(λyx.xzy)(λx.x)x")("xz(λx.x)")
    parseAndReduce("beta reduce 6")("xz(λx.x)")("xz(λx.x)")
    parseAndReduce("beta reduce 7")("λx.yx")("y")
    parseAndReduce("beta reduce 8")("(λx.(λy.x)y)z")("z")
    
  }
  
  @main def runValidate = {
    def validate(s: String)(expected: String): Unit = parseExpr(s)(s).map{ x => x match { case (_, t) => asStr(false)(t) } }.map(s2 => println(s"$s == $s2 => ${s2==expected}"))
    validate("λx.x")("λx.x")
    validate("(λx.x)y")("(λx.x)y")
    validate("(λx.xy)")("λx.xy")
    validate("(λx.(λy.xy))y")("(λxy.xy)y")
    validate("(λx.(λy.(x(λx.xy))))y")("(λxy.x(λx.xy))y")
    validate("(λx.xx)(λx.x)")("(λx.xx)(λx.x)")
    println("done.")
  }
  
  @main def numbers = {
    object λ:
      
      def parse: String => Option[ParserResult[LcExpr]] = exprP.parse
      
      def run: LcExpr => LcExpr = expr => {
        val fp = fingerprint(expr)
        val beta = betaReduce(fp)
        val eta = etaConvertReduce(beta)
        eta
      }

      def parseRun: String => Option[String] = expr => for {
        (_, t) <- parse(expr)
        result = run(t)
      } yield asStr(false)(result)

      def lcVar(v: Char): LcVar = LcVar(v.toString)
      def func(v: Char, expr: LcExpr => LcExpr): LcExpr = lcVar(v) pipe (p => LcFunc(p, expr(p)))
      def ident(v: Char): LcExpr = func(v, identity)
      def zero = func('s', s => ident('z'))
      def succ = func('w', w => func('y', y => func('x', x => y app (w app y app x))))
      def mul = func('x', x => func('y', y => func('z', z => x app (y app z))))
      
      def num(v: Int): LcExpr = v match {
        case 1 => succ app zero
        case n => succ app num(n-1)
      }
    
      extension (left: LcExpr)
        infix def app (right: LcExpr): LcExpr =
          LcApp(left, right)
        def +(right: LcExpr): LcExpr = left app succ app right
        def *(right: LcExpr): LcExpr = mul app left app right
          
    end λ
    import λ._

    (num(2) + num(3)) pipe run tap (n => println(s"2 + 3 = ${asStr(false)(n)}"))
    (num(2) * num(3)) pipe run tap (n => println(s"2 * 3 = ${asStr(false)(n)}"))
//    
//    def thing = for {
//      _ = println(s"s = ${asStr(false)(succ)}")
//      _ = println(s"s = ${asStr(false)(mul)}")
//      _ = println(s"0 = ${asStr(false)(zero)}")
//      one = succ app zero pipe run tap (n => println(s"1 = ${asStr(false)(n)}"))
//      two = succ app one pipe run tap (n => println(s"2 = ${asStr(false)(n)}"))
//      three = succ app two pipe run tap (n => println(s"3 = ${asStr(false)(n)}"))
//      four = succ app three pipe run tap (n => println(s"4 = ${asStr(false)(n)}"))
//      five = succ app four pipe run tap (n => println(s"5 = ${asStr(false)(n)}"))
//      ss3 = (succ app (succ app three)) pipe run tap (n => println(s"ss3 = ${asStr(false)(n)}"))
//      twoPlusThree = (two app succ app three) pipe run tap (n => println(s"2s3 = ${asStr(false)(n)}"))
//      twoTimesThree = (mul app two app three) pipe run tap (n => println(s"2x3 = ${asStr(false)(n)}"))
//    } yield ()
//    thing
  }
}