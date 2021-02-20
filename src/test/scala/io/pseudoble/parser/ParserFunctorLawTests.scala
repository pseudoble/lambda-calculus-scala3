package io.pseudoble.parser

import io.pseudoble.effects.typeclasses._
import io.pseudoble.parser._
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.lambda._
//import io.pseudoble.lambda.LcExpr._
import io.pseudoble.tools._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ParserFunctorLawTests extends AnyFlatSpec with Checkers {
  opaque type IntToStr = Int => String
  opaque type StringToInt = String => Int

  given Arbitrary[Parser[Int]] = Arbitrary {
    for {
      remaining <- Gen.asciiPrintableStr
      value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    } yield Parser[Int] { _ => (Some(remaining, value)) }
  }
  given Arbitrary[IntToStr] = Arbitrary {
    for {
      value <- Gen.asciiPrintableStr
      //      value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    } yield (x: Int) => value
  }
  given Arbitrary[StringToInt] = Arbitrary {
    for {
      value <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    } yield (x: String) => value
  }
  
  "map" should "preserve identity morphisms" in {
    check(identityLaw)
  }
  "map" should "preserve composition of morphisms" in {
    check(compositionLaw)
  }

  def identityLaw(a: Parser[Int]): Prop = forAll { (stream: String) =>
    a.parse(stream) == a.map(identity).parse(stream)
  } 
  
  def compositionLaw(a: Parser[Int]): Prop = forAll { (stream: String, i2s: IntToStr, s2i: StringToInt) =>
    a.map(i2s).map(s2i).parse(stream) == a.map(i2s andThen s2i).parse(stream)
  }
}