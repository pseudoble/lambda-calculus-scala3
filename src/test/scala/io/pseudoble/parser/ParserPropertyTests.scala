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

class ParserPropertyTests extends AnyFlatSpec with Checkers {
  val invalidVarCharsSet = Set('λ','.',' ','\t', '(',')')
  val invalidVarChar = Gen.oneOf(invalidVarCharsSet)
  val validVarChar = Gen.asciiPrintableChar.retryUntil(!invalidVarCharsSet.contains(_),10)
  
  val streamStartingWithValidVar: Gen[String] = for {
    value <- validVarChar
    rest <- Gen.asciiPrintableStr
  } yield s"$value$rest"
  
  val streamStartingWithInvalidVar: Gen[String] = for {
    value <- invalidVarChar
    rest <- Gen.asciiPrintableStr
  } yield s"$value$rest"
//  
//  "varP" should "succeed when first char in stream is valid var" in {
//    check(forAll(streamStartingWithValidVar) { stream =>
//      varP.parse(stream).isDefined
//    })
//  }
//  "varP" should "fail when first char in  stream is invalid var" in {
//    check(forAll(streamStartingWithInvalidVar) { stream =>
//      varP.parse(stream).isEmpty
//    })
//  }
}