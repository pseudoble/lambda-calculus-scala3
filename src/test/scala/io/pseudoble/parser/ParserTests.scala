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

class ParserTests extends AnyFlatSpec {
  "charP" should "return Some with remaining stream and requested char on success" in {
    val result = charP('c').parse("charles")
    result should not be empty
    result should contain(("harles", 'c'))
  }
  "charP" should "return None when expected char was not at head of stream" in {
    val result = charP('x').parse("charles")
    result shouldBe empty
  }
  "charP" should "work with lambda symbol" in {
    val result = charP('λ').parse("λx.x")
    result should not be empty
    result should contain(("x.x", 'λ'))
  }

  "stringP" should "return Some with remaining stream and requested string on success" in {
    val result = stringP("char").parse("charles")
    result should not be empty
    result should contain(("les", "char"))
  }
  "stringP" should "return None when expected string was not at head of stream" in {
    val result = stringP("char").parse("changed")
    result shouldBe empty
  }
  
  "manyP" should "return Some with remaining stream and a list of the inner Parser's results on success" in {
    val result = many(charP('a')).parse("aardvark")
    result should not be empty
    result should contain (("rdvark", List('a', 'a')))
  }
  "manyP" should "return Some with entire stream and an empty list when inner parser never matches" in {
    val result = many(charP('a')).parse("bovine")
    result should not be empty
    result should contain (("bovine", List()))
  }

  "atLeast" should "return Some with remaining stream and a list of the inner Parser's results on successfully matching at least the minimum" in {
    val result = BasicParsers.atLeast(2)(charP('a')).parse("aardvark")
    result should not be empty
    result should contain (("rdvark", List('a', 'a')))
  }
  "atLeast" should "return None when minimum count is not reached" in {
    val result = BasicParsers.atLeast(3)(charP('a')).parse("aardvark")
    result shouldBe empty
  }

  "opt" should "return Some with Option containing result if successful" in {
    val result = opt(charP('b')).parse("bovine")
    result should not be empty
    result should contain (("ovine", Some('b')))
  }
  "opt" should "return Some with full stream and None Option if not successful" in {
    val result = opt(charP('a')).parse("bovine")
    result should not be empty
    result should contain (("bovine", None))
  }

  "productR" should "parse both sides but return the right side successfully" in {
    val result = (charP('a') :*> charP('b')).parse("abc")
    result should not be empty
    result should contain (("c", 'b'))
  }

  "productL" should "parse both sides but return the left side successfully" in {
    val result = (charP('a') <*: charP('b')).parse("abc")
    result should not be empty
    result should contain (("c", 'a'))
  }
  
  "varP" should "parse a single character as an LcVar successfully" in {
    val result = varP.parse("xyz")
    result should not be empty
    result should contain (("yz", LcVar("x")))
  }

  "varP" should "fail on lambda symbols" in {
    val result = varP.parse("λx.y")
    result shouldBe empty
  }

  "varP" should "fail on periods" in {
    val result = varP.parse(".abc")
    result shouldBe empty
  }

  "varP" should "fail on open parens" in {
    val result = varP.parse("(abc)")
    result shouldBe empty
  }

  "varP" should "fail on close parens" in {
    val result = varP.parse(")ok")
    result shouldBe empty
  }

  "varP" should "fail on space" in {
    val result = varP.parse(" ok")
    result shouldBe empty
  }

  "appP" should "return None when parsing an empty stream" in {
    val result = appP.parse("")
    result shouldBe empty
  }

  "appP" should "return None when parsing a stream with just one var" in {
    val result = appP.parse("a")
    result shouldBe empty
  }

  "appP" should "parse a pair of vars as a single LcApp successfully" in {
    val result = appP.parse("xy")
    result should not be empty
    result should contain (("", LcApp(LcVar("x"), LcVar("y"))))
  }
//
//  "appP" should "return None when parsing a stream with a simple func in it" in {
//    val result = appP.parse("λx.x")
//    result shouldBe empty
//  }
//
//  "appP" should "return None when parsing a stream with a func in it that contains an application expression" in {
//    val result = appP.parse("λx.xy")
//    result shouldBe empty
//  }
//
//  "appP" should "parse a series of many vars as a series of nested LcApp expressions with the order correct successfully" in {
//    val result = appP.parse("abcdef")
//    result should not be empty
//    result should contain (("", LcApp(LcApp(LcApp(LcApp(LcApp(LcVar("a"), LcVar("b")), LcVar("c")), LcVar("d")), LcVar("e")), LcVar("f"))))
//  }
  
  "funcP" should "return a valid LcFunc when parsing a stream with a simple func in it" in {
    val result = funcP.parse("λx.y")
    result should not be empty
    result should contain (("", LcFunc(LcVar("x"), LcVar("y"))))
  }
}