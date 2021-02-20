//package io.pseudoble.playground
//
//import scala.util.chaining.scalaUtilChainingOps
//
//import io.pseudoble.effects.typeclasses._
//import io.pseudoble.parser.BasicParsers._
//import io.pseudoble.parser._
//import io.pseudoble.lambda._
//import io.pseudoble.lambda.types._
//import io.pseudoble.lambda.LcParser.parser
//import io.pseudoble.tools._
//
//
//object tryOuts {
//
//  @main def tryOperators  =   {
//    def runAndPrint[A](p: Parser[A]) = p.parse("charles") tap println
//
//    def mapper(b: Int)(a: Char) = b.toHexString.head
//    def flatMapper(b: Int)(a: Char)(using parserAp: Applicative[Parser]) = parserAp.pure(b.toHexString.head)
//
//    val subject = charP('c')
//
//    subject.map(c => c.toUpper) tap runAndPrint
//
//    (subject map mapper(1)) tap runAndPrint    
//    (subject >>= flatMapper(1)) tap runAndPrint
//
//    val parserAp: Monad[Parser] = summon[Monad[Parser]]
//
//    val f: String => Option[Int] = _.toIntOption
//    val input = "42"
//    val inputParser = stringP(input)
//
//    def parse42asIntOption_manual: Parser[Option[Int]] = Parser { input =>
//      inputParser.parse(input) match {
//        case Some((s, value)) => Some((s, f(value)))
//        case None => None
//      }
//    }
//
//    val liftedF = parserAp.lift(f)
//    val parse42asIntOption_lifted = liftedF(inputParser)
//
//    val pureF = parserAp.pure(f)
//    val parse42asIntOption_pure = (inputParser <*> pureF)
//
//    parse42asIntOption_manual.parse(input) tap println
//    parse42asIntOption_lifted.parse(input) tap println
//    parse42asIntOption_pure.parse(input) tap println
//
//    { charP('c') :*> charP('h') }.parse("charles") tap println
//    { charP('c') <*: charP('h') }.parse("charles") tap println
//
////    val pVars = charP('λ') :*> many(varP)
////    val pDotExpr = charP('.') :*> exprP
////    val pFunc = pVars * pDotExpr
////    val pFunc2 = (charP('λ') :*> many(varP)) * (charP('.') :*> exprP)
////
////    pVars.parse("abc") tap (r => println(s"$r should be None"))
////    pVars.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some((".xyz",List(Identifier("a"), Identifier("b"), Identifier("c")))) }"))
////    pDotExpr.parse("λabc.xyz") tap (r => println(s"$r should be None"))
////    pDotExpr.parse(".xyz") tap (r => println(s"$r => ${ r == Some(("",App(App(Identifier("x"), Identifier("y")), Identifier("z")))) }"))
////    pFunc.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("",App(App(Identifier("x"), Identifier("y")), Identifier("z")))) }"))
////    pFunc2.parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("",App(App(Identifier("x"), Identifier("y")), Identifier("z")))) }"))
////
////    pFunc2
////      .map{ (l,r) => l.foldRight(r)(LcFunc(_, _)) }
////      .parse("λabc.xyz") tap (r => println(s"$r => ${ r == Some(("", LcFunc(Identifier("a"), LcFunc(Identifier("b"), LcFunc(Identifier("c"), App(App(Identifier("x"), Identifier("y")), Identifier("z"))))))) }"))
//  }
//
//  def parseAndPrint[T](p: Parser[T])(label: String): ParserFunc[T] = (s: String) => p.parse(s) tap {
//    case Some((r, tree)) => println(s"[$r] $label: $s => $tree")
//    case None => println("Parsing failed")
//  }
//  val parseExpr = parseAndPrint(parser)
//
//
//  @main def tryParser = {
//
//    parseExpr("simplest expr")("x")
//    parseExpr("simplest app")("xy")
//    parseExpr("compound app")("xyz")
//
//    val basicFunction = parseExpr("basic function")("λx.x")
//    val longForm = parseExpr("full curried")("λx.λy.x")
//    val shortForm = parseExpr("short curried")("λxy.x")
//    longForm == shortForm tap println
//
//    parseExpr("short curried, app in body")("λxy.xy")
//    parseExpr("same as previous, with explicit parens changing order of operations")("(λxy.x)y")
//
//
//    def thing: ParserResult[Expr] => Expr = r => r match { case (_, t) => t tap (v => println(asStr(v))) }
//    val fancyParse = parseExpr("book page 3 bottom")("(λx.(λx.λy.xyz))y")
//    fancyParse.map(thing)
//
//    val fancyParse2 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.x(yz)))y")
//    fancyParse2.map(thing)
//
//    val fancyParse3 = parseExpr("book page 3 bottom different")("(λx.(λx.λy.xyz(λz.z)x))y")
//    fancyParse3.map(thing)
//
//
//    parseExpr("paren test 1 - expected 'abc'")("abc")
//      .map(thing)
//    parseExpr("paren test 1.a - expected 'abc'")("(ab)c")
//      .map(thing)
//    parseExpr("paren test 2 - expected 'a(bc)'")("a(bc)")
//      .map(thing)
//    parseExpr("paren test 3 - expected '(λx.x)y'")("(λx.x)y")
//      .map(thing)
//    parseExpr("paren test 4 - expected '(λx.x)(yz)'")("(λx.x)(yz)")
//      .map(thing)
//    parseExpr("paren test 5 - expected '(λx.x)λx.x'")("(λx.x)(λx.x)")
//      .map(thing)
//    parseExpr("paren test 6 - expected '(λx.x)λx.x'")("λx.x(λx.x)")
//      .map(thing)
//    parseExpr("param test 1 - expected 'λxy.xy'")("λx.λy.xy")
//      .map(thing)
//    parseExpr("param test 1 - expected 'λx.(λy.x)y'")("λx.(λy.x)y")
//      .map(thing)
//    parseExpr("param test 1 - expected '(λx.xx)(λx.x)'")("(λx.xx)(λx.x)")
//      .map(thing)
//
//    println("---------------------------------------")
//
//
//    def parseAndReduce(label: String)(input: String)(expected: String): Option[Expr] = for {
//      (_, parsed) <- parser(input)
////      _ = printPretty(parsed)
//      betaReduced = reduce(parsed)
////      _ = printPretty(betaReduced)
//      etaReduced = betaReduced //etaConvertReduce(betaReduced)
////      _ = printPretty(etaReduced)
//      success = asStr(etaReduced) == expected
//      _ = println(s"[$success] $input | ${asStr(parsed)} | ${asStr(betaReduced)} | ${asStr(etaReduced)} == $expected")
////      _ = println("-------")
//    } yield etaReduced
//
//    parseAndReduce("beta reduce 1")("(λx.x) y")("y")
//    parseAndReduce("beta reduce 2")("(λx.x x x) (λy.y)")("λy.y")
//    parseAndReduce("beta reduce 3")("(λx y.x y) (λy.y)")("λy.y")
//    parseAndReduce("beta reduce 4")("(λx y z.x y z) (λx y.x y) (λx.x) x")("x")
//    parseAndReduce("beta reduce 5")("(λx y z.x y z) (λy x.x z y) (λx.x) x")("x z (λx.x)")
//    parseAndReduce("beta reduce 6")("x z (λx.x)")("x z (λx.x)")
//    parseAndReduce("beta reduce 7")("λx.y x")("y")
//    parseAndReduce("beta reduce 8")("(λx.(λy.x) y) z")("z")
//    parseAndReduce("alpha convert 1")("(λx y.x y) y")("λy.y y")
//
//  }
//
//  @main def runValidate = {
//    def validate(s: String)(expected: String): Unit = parseExpr(s)(s).map{ x => x match { case (_, t) => asStr(t) } }.map(s2 => println(s"$s == $s2 => ${s2==expected}"))
//    validate("λx.x")("λx.x")
//    validate("(λx.x) y")("(λx.x) y")
//    validate("(λx.x y)")("λx.(x y)")
//    validate("(λx.(λy.x y)) y")("(λx y.(x y)) y")
//    validate("(λx.(λy.(x (λx.x y)))) y")("(λx y.(x (λx.(x y)))) y")
//    validate("(λx.x x) (λx.x)")("(λx.(x x)) (λx.x)")
//    validate("(λcat.(λdog.(cat (λcat.cat dog)))) dog")("(λcat dog.(cat (λcat.(cat dog)))) dog")
////    validate("λx.* x 3")("λx.* x 3")
//    println("done.")
//  }
////
////  @main def numbers = {
////    object λ:
////
////      def parse: String => Option[ParserResult[Expr]] = parser.parse
////
////      def parseRun: String => Option[String] = expr => for {
////        (_, t) <- parse(expr)
////        result = run(t)
////      } yield asStr(result)
////
////      private def parseUnsafe(str: String): Expr = parse(str) match {
////        case Some(("", expr)) => expr
////        case Some((stream, _)) => throw Exception(s"Parsing failed for '$str': Stream was not fully parsed: '$stream' remained.")
////        case None => throw Exception(s"Parsing failed for '$str'. Parser returned None")
////      }
////
////      def run: Expr => Expr = expr => {
////        val beta = betaReduce(expr)
////        val eta = etaConvertReduce(beta)
////        eta
////      }
////
////      val I: Expr = "(λx.x)" |> parseUnsafe
////      val S: Expr = s"(λwyx.y(wyx))" |> parseUnsafe
////      val ZERO: Expr = s"(λsz.z)" |> parseUnsafe
////      val MUL: Expr = s"(λxyz.x(yz))" |> parseUnsafe
////
////      def num(v: Int): Expr = v match {
////        case 1 => S <*> ZERO
////        case n => S <*> num(n-1)
////      }
////
////      def ap: Expr => Expr => Expr = 
////        left => right => Expr.App(left, right)
////
////      def add: Expr => Expr => Expr =
////        left => right => left <*> S <*> right
////
////      def mul: Expr => Expr => Expr =
////        left => right => MUL <*> left <*> right
////
////      def Identifier(v: Char): Expr.Identifier = Expr.Identifier(v.toString)
////      def func(v: Char, expr: Expr => Expr): Expr = Identifier(v) pipe (p => Expr.Func(p, expr(p)))
////
////
////      extension (left: Expr)
////        def <*>(right: Expr): Expr = ap(left)(right)
////        def +(right: Expr): Expr = add(left)(right)
////        def *(right: Expr): Expr = mul(left)(right)
////
////    end λ
////    import λ._
////
////    (num(2) + num(3)) pipe run tap (n => println(s"2 + 3 = ${asStr(n)}"))
////    (num(2) * num(3)) pipe run tap (n => println(s"2 * 3 = ${asStr(n)}"))
//////    
//////    def thing = for {
//////      _ = println(s"s = ${asStr(succ)}")
//////      _ = println(s"s = ${asStr(mul)}")
//////      _ = println(s"0 = ${asStr(zero)}")
//////      one = succ app zero pipe run tap (n => println(s"1 = ${asStr(n)}"))
//////      two = succ app one pipe run tap (n => println(s"2 = ${asStr(n)}"))
//////      three = succ app two pipe run tap (n => println(s"3 = ${asStr(n)}"))
//////      four = succ app three pipe run tap (n => println(s"4 = ${asStr(n)}"))
//////      five = succ app four pipe run tap (n => println(s"5 = ${asStr(n)}"))
//////      ss3 = (succ app (succ app three)) pipe run tap (n => println(s"ss3 = ${asStr(n)}"))
//////      twoPlusThree = (two app succ app three) pipe run tap (n => println(s"2s3 = ${asStr(n)}"))
//////      twoTimesThree = (mul app two app three) pipe run tap (n => println(s"2x3 = ${asStr(n)}"))
//////    } yield ()
//////    thing
////  }
//
//  
//}