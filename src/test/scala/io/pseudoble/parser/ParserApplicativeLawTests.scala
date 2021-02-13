package io.pseudoble.parser

import io.pseudoble.effects.typeclasses._
import io.pseudoble.parser._
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.lambda._
import io.pseudoble.lambda.LcExpr._
import io.pseudoble.tools._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ParserApplicativeLawTests extends AnyFlatSpec with Checkers {
  "pure" should "resolve expected value" in {
    check(pureLawish)
  }
  def pureLawish: Prop = forAll { (stream: String, value: Int) =>
    summon[Applicative[Parser]].pure(value).parse(stream) == Some((stream, value))
  }

  val parserInt: Gen[(Int , Parser[Int])] = for {
    value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
  } yield (value, summon[Applicative[Parser]].pure(value))

  val parserF: Gen[(Int => Int, Parser[Int => Int])] = for {
    f <- Gen.oneOf[Int => Int](
      x => x+x,
      x => x*x,
      x => -x,
      x => x)
  } yield (f, summon[Applicative[Parser]].pure(f))

  val arbitraryStream = Gen.asciiPrintableStr
  
  def checkValue[T](work: => Option[ParserResult[T]], assertion: T => Boolean): Boolean =
    work match {
      case Some((_, actualValue)) => assertion(actualValue)
      case _ => false
    }
  
  "ap" should "work as expected" in {
    check(forAll(arbitraryStream, parserF, parserInt) { 
      case (stream, (f, pf), (i, pi)) =>
        checkValue((pi ap pf).parse(stream), _ == f(i))
    })
  }

  "product" should "work as expected" in {
    check(forAll(arbitraryStream, parserInt, parserInt) {
      case (stream, (ai, ap), (bi, bp)) =>
        checkValue((ap product bp).parse(stream), _ == (ai, bi))
    })
  }

  "productL" should "select the left value" in {
    check(forAll(arbitraryStream, parserInt, parserInt) {
      case (stream, (ai, ap), (bi, bp)) =>
        checkValue((ap <*: bp).parse(stream), _ == ai)
    })
  }
  
  "productR" should "select the right value" in {
    check(forAll(arbitraryStream, parserInt, parserInt) {
      case (stream, (ai, ap), (bi, bp)) =>
        checkValue((ap :*> bp).parse(stream), _ == bi)
    })
  }
}