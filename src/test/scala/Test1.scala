
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
  
  "varP" should "succeed when first char in stream is valid var" in {
    check(forAll(streamStartingWithValidVar) { stream =>
      varP.parse(stream).isDefined
    })
  }
  "varP" should "fail when first char in  stream is invalid var" in {
    check(forAll(streamStartingWithInvalidVar) { stream =>
      varP.parse(stream).isEmpty
    })
  }
}

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