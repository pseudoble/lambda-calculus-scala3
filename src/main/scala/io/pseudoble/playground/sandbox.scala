package io.pseudoble.playground

import io.pseudoble.effects.typeclasses._
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.parser._
import io.pseudoble.lambda._
import io.pseudoble.lambda.LcExpr._
import io.pseudoble.tools._

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


    def thing: ParserResult[LcExpr] => LcExpr = r => r match { case (_, t) => t tap (v => println(asStr(v))) }
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
      betaReduced = betaReduce(parsed)
//      _ = printPretty(betaReduced)
      etaReduced = etaConvertReduce(betaReduced)
//      _ = printPretty(etaReduced)
      success = asStr(etaReduced) == expected
      _ = println(s"[$success] $input -> ${asStr(parsed)} >> ${asStr(betaReduced)} >> ${asStr(etaReduced)} == $expected")
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
    parseAndReduce("alpha convert 1")("(λxy.xy)y")("λy.yy")
    
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
  
  @main def numbers = {
    object λ:
      
      def parse: String => Option[ParserResult[LcExpr]] = exprP.parse

      def parseRun: String => Option[String] = expr => for {
        (_, t) <- parse(expr)
        result = run(t)
      } yield asStr(result)
      
      private def parseUnsafe(str: String): LcExpr = parse(str) match {
        case Some(("", expr)) => expr
        case Some((stream, _)) => throw Exception(s"Parsing failed for '$str': Stream was not fully parsed: '$stream' remained.")
        case None => throw Exception(s"Parsing failed for '$str'. Parser returned None")
      }
      
      def run: LcExpr => LcExpr = expr => {
        val beta = betaReduce(expr)
        val eta = etaConvertReduce(beta)
        eta
      }
      
      val I: LcExpr = "(λx.x)" |> parseUnsafe
      val S: LcExpr = s"(λwyx.y(wyx))" |> parseUnsafe
      val ZERO: LcExpr = s"(λsz.z)" |> parseUnsafe
      val MUL: LcExpr = s"(λxyz.x(yz))" |> parseUnsafe
      
      def num(v: Int): LcExpr = v match {
        case 1 => S <*> ZERO
        case n => S <*> num(n-1)
      }

      def ap: LcExpr => LcExpr => LcExpr = 
        left => right => LcApp(left, right)
        
      def add: LcExpr => LcExpr => LcExpr =
        left => right => left <*> S <*> right

      def mul: LcExpr => LcExpr => LcExpr =
        left => right => MUL <*> left <*> right

      def lcVar(v: Char): LcVar = LcVar(v.toString)
      def func(v: Char, expr: LcExpr => LcExpr): LcExpr = lcVar(v) pipe (p => LcFunc(p, expr(p)))
      
    
      extension (left: LcExpr)
        def <*>(right: LcExpr): LcExpr = ap(left)(right)
        def +(right: LcExpr): LcExpr = add(left)(right)
        def *(right: LcExpr): LcExpr = mul(left)(right)
          
    end λ
    import λ._

    (num(2) + num(3)) pipe run tap (n => println(s"2 + 3 = ${asStr(n)}"))
    (num(2) * num(3)) pipe run tap (n => println(s"2 * 3 = ${asStr(n)}"))
//    
//    def thing = for {
//      _ = println(s"s = ${asStr(succ)}")
//      _ = println(s"s = ${asStr(mul)}")
//      _ = println(s"0 = ${asStr(zero)}")
//      one = succ app zero pipe run tap (n => println(s"1 = ${asStr(n)}"))
//      two = succ app one pipe run tap (n => println(s"2 = ${asStr(n)}"))
//      three = succ app two pipe run tap (n => println(s"3 = ${asStr(n)}"))
//      four = succ app three pipe run tap (n => println(s"4 = ${asStr(n)}"))
//      five = succ app four pipe run tap (n => println(s"5 = ${asStr(n)}"))
//      ss3 = (succ app (succ app three)) pipe run tap (n => println(s"ss3 = ${asStr(n)}"))
//      twoPlusThree = (two app succ app three) pipe run tap (n => println(s"2s3 = ${asStr(n)}"))
//      twoTimesThree = (mul app two app three) pipe run tap (n => println(s"2x3 = ${asStr(n)}"))
//    } yield ()
//    thing
  }
  
  @main def alphaConvert = {
    /* 
* (λxy.xy)y would result in λy.yy
* this is no-bueno
* 
*   λ.M --> λx0.M[x --> x0]
*   λ.M <-- λx0.M[x <-- x0]
*   λ.M <-> λx0.M[x <-> x0]
* where x0 is not allowed to be a free variable in M.
* The process of alpha conversion may not alter the value of the expression.
* The expression to be converted (M) and the converted result (N) are said 
* to be equal "modulo alpha".
* 
* (λxy.xy)y →β (λy0.yy0)
* 
* */
    def findSimpleCollision(leftS: String)(rightS: String): Unit = {
      def startSearch(target: LcExpr, replacement: LcExpr): Either[List[String], Unit] = {
        def searchForCollision(replacementName: String, targetExpr: LcExpr)(errsAcc: List[String]): Either[List[String], Unit] = {
          targetExpr match {
            case f @ LcFunc(LcVar(`replacementName`), _) => Left {
              s"!!! found replacement name '$replacementName' as λ param declaration in '${asStr(f)}'. This will require alpha conversion..." +: errsAcc
            }
            case f @ LcFunc(_, subBody) => searchForCollision(replacementName, subBody)(errsAcc)
            case LcApp(left, right) =>
              val leftResults = searchForCollision(replacementName, left)(List())
              val rightResults = searchForCollision(replacementName, right)(List())
              (leftResults, rightResults) match {
                case (Left(leftErrs), Left(rightErrs)) => Left(leftErrs ++ rightErrs ++ errsAcc)
                case (Left(leftErrs), _) => Left(leftErrs ++ errsAcc)
                case (_, Left(rightErrs)) => Left(rightErrs ++ errsAcc)
                case _ => Right(())                  
              }
            case _: LcVar => Right(())
          }
        }

        def getBindingToReplace(inExpr: LcExpr): Either[String, (String, LcExpr)] = inExpr match {
          case LcFunc(LcVar(targetBindingName), inTargetExpression) => Right { (targetBindingName, inTargetExpression) }
          case unhandledTarget => Left { s"??? Not sure what to do with target ${asStr(unhandledTarget)}" }
        }
        
        def getReplacementName(inExpr: LcExpr): Either[String, String] = inExpr match {
          case LcVar(replacementName) => Right { replacementName }
          case unhandledReplacement => Left { s"??? Not sure what to do with replacement ${asStr(unhandledReplacement)}" }
        }

        for {
          toReplace <- getBindingToReplace(target).left.map((msg: String) => List(msg))
          (targetName, targetExpr) = toReplace
          
          replacementName <- getReplacementName(replacement).left.map((msg: String) => List(msg))
          res <- if (targetName == replacementName)
            then Right(())
            else searchForCollision(replacementName, targetExpr)(List())
        } yield res 
      }
      val result = for {
        leftResult <- exprP.parse(leftS).toRight(List("Parsing left expression failed"))
        rightResult <- exprP.parse(rightS).toRight(List("Parsing right expression failed"))
        ((_, left),(_, right)) = (leftResult, rightResult)
        _ <- startSearch(left, right)
        _ = startSearch(right, left)
      } yield () 
      
      result match {
        case Left(errs: List[String]) => println(errs.mkString("\n"))
        case Right(()) => println("No issues found.")
      }
    }

    findSimpleCollision("λx.x")("y")
    findSimpleCollision("λy.y")("y")
    findSimpleCollision("λxy.y")("y")
  }
}