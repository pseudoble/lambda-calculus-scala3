package io.pseudoble.parser

import scala.util.chaining.scalaUtilChainingOps
import io.pseudoble.effects.typeclasses._
import io.pseudoble.effects.typeclasses.Extensions._
import io.pseudoble.parser._
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.lambda._
import io.pseudoble.tools._
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.matchers.dsl

trait LcShorthand {
  import types.Expr
  import types.Expr._

  def Fn(binding: String, body: Expr) = Func(Binding(binding), body)
  def Ap = App(_, _)
  def Ref = BindingRef(_)
  def FV = FreeVar(_)
}

class BasicParserTests extends AnyFlatSpec with OptionValues {
  "charP" should "return Some with remaining stream and requested char on success" in {
    val result = charP('c').parse("charles")
    result.value should be (("harles", 'c'))
  }
  
  "charP" should "return None when expected char was not at head of stream" in {
    val result = charP('x').parse("charles")
    result shouldBe empty
  }
  
  "charP" should "work with lambda symbol" in {
    val result = charP('λ').parse("λx.x")
    result.value should be(("x.x", 'λ'))
  }

  "stringP" should "return Some with remaining stream and requested string on success" in {
    val result = stringP("char").parse("charles")
    result.value should be(("les", "char"))
  }
  
  "stringP" should "return None when expected string was not at head of stream" in {
    val result = stringP("char").parse("changed")
    result shouldBe empty
  }

  "manyP" should "return Some with remaining stream and a list of the inner Parser's results on success" in {
    val result = many(charP('a')).parse("aardvark")
    result.value should be(("rdvark", List('a', 'a')))
  }
  
  "manyP" should "return Some with entire stream and an empty list when inner parser never matches" in {
    val result = many(charP('a')).parse("bovine")
    result.value should be(("bovine", List()))
  }

  "atLeast" should "return Some with remaining stream and a list of the inner Parser's results on successfully matching at least the minimum" in {
    val result = BasicParsers.atLeast(2)(charP('a')).parse("aardvark")
    result.value should be(("rdvark", List('a', 'a')))
  }
  
  "atLeast" should "return None when minimum count is not reached" in {
    val result = BasicParsers.atLeast(3)(charP('a')).parse("aardvark")
    result shouldBe empty
  }

  "opt" should "return Some with Option containing result if successful" in {
    val result = opt(charP('b')).parse("bovine")
    result.value should be(("ovine", Some('b')))
  }
  "opt" should "return Some with full stream and None Option if not successful" in {
    val result = opt(charP('a')).parse("bovine")
    result.value should be(("bovine", None))
  }

  "productR" should "parse both sides but return the right side successfully" in {
    val result = (charP('a') :*> charP('b')).parse("abc")
    result.value should be(("c", 'b'))
  }

  "productL" should "parse both sides but return the left side successfully" in {
    val result = (charP('a') <*: charP('b')).parse("abc")
    result.value should be(("c", 'a'))
  }
}

class LambdaParserTests extends AnyFlatSpec with EitherValues with LcShorthand {
  import types.Expr._

  "a single string of characters" should "parse as a single FreeVar successfully" in {
    val result = LcParser.parse("xyz") 
    result.value should be (FreeVar("xyz"))
  }

  "identity func" should "parse as indexed identity func" in {
    val result = LcParser.parse("\\x.x")
    result.value should be (Func(Binding("x"), BindingRef(0)))
  }

  "simple func with free variable" should "parse as func with FreeVar" in {
    val result = LcParser.parse("\\x.y")
    result.value should be (Func(Binding("x"), FreeVar("y")))
  }
  
  "nested func with var body of second param" should "parse as func with BindingRef(0)" in {
    val result = LcParser.parse("\\x.\\y.y")
    result.value should be (Func(Binding("x"), Func(Binding("y"), BindingRef(0))))
  }
  
  "nested func with var body of first param" should "parse as func with BindingRef(1)" in {
    val result = LcParser.parse("\\x.\\y.x")
    result.value should be (Func(Binding("x"), Func(Binding("y"), BindingRef(1))))
  }
  
  "apply with two free variables" should "parse as apply with FreeVars" in {
    val result = LcParser.parse("left right")
    result.value should be (App(FreeVar("left"), FreeVar("right")))
  }

  "a b c" should "parse as (a b) c" in {
    val result = LcParser.parse("a b c")
    result.value should be (App(App(FreeVar("a"), FreeVar("b")), FreeVar("c")))
  }

  "(a b) c" should "parse as (a b) c" in {
    val result = LcParser.parse("(a b) c")
    result.value should be (App(App(FreeVar("a"), FreeVar("b")), FreeVar("c")))
  }

  "a (b c)" should "parse as a (b c)" in {
    val result = LcParser.parse("a (b c)")
    result.value should be (App(FreeVar("a"), App(FreeVar("b"), FreeVar("c"))))
  }

  "(a b) (c d)" should "parse as (a b) (b c)" in {
    val result = LcParser.parse("(a b) (c d)")
    result.value should be (App(App(FreeVar("a"), FreeVar("b")), App(FreeVar("c"), FreeVar("d"))))
  }

  "\\a.b c" should "parse as lambda with apply of b and c in body" in {
    val result = LcParser.parse("\\a.b c")
    result.value should be (Func(Binding("a"), App(FreeVar("b"), FreeVar("c"))))
  }

  "\\a.b \\c.c" should "parse as equivalent to \\a.(b \\c.c)" in {
    val result = LcParser.parse("\\a.b \\c.c")
    result.value should be (Func(Binding("a"), App(FreeVar("b"), Func(Binding("c"), BindingRef(0)))))
  }

  "(\\a.b) \\c.c" should "parse as equivalent to (\\a.b) \\c.c" in {
    val result = LcParser.parse("(\\a.b) \\c.c")
    result.value should be (App(Func(Binding("a"), FreeVar("b")), Func(Binding("c"), BindingRef(0))))
  }

  "\\a.b (\\c.c)" should "parse as equivalent to \\a.(b \\c.c)" in {
    val result = LcParser.parse("\\a.b (\\c.c)")
    result.value should be (Func(Binding("a"), App(FreeVar("b"), Func(Binding("c"), BindingRef(0))))) 
  }
}
 
class LambdaReduceTests extends AnyFlatSpec with Inside with LcShorthand {
  import types.Expr
  import types.Expr._
  import org.scalactic.Prettifier._

  //  "sandbox" should "run" in {
  //    def runS(input: String): Unit = LcParser.parse(input) map runE
  //    def runE(input: Expr): Unit = (input |> reduce |> asStr) tap (result => println(s"sandbox: '${asStr(input)}' => '$result'"))
  //
  //    runS("(\\x.\\y.(x y)) y")
  //    runS("(\\x.\\y.\\z.(x y z)) (y z)")
  //    runS("(\\x.\\y.\\z.(x y z)) (\\x.y z)")
  //    runE(App(Func(Binding("x"), Func(Binding("y"), Func(Binding("z"), App(App(BindingRef(2), BindingRef(1)), BindingRef(0))))), 
  //      Func(Binding("x"), App(BindingRef(0), BindingRef(1)))))
  //  } 

  // (\x.x) (<0> <1>) is like....
  //     => \A.\B.(\x.x) (B A)
  //     => \A.\B.(\x.<0>) (<0> <1>) 
  //     => \A.\B.(<0> <1>)
  //     => \A.\B.(B A)
  //     => indexes did not shift because the target in \x was at the root of \x

  "applications with higher-level BindingRefs in the right operand" should 
    "not increment the higher-level BindingRefs if the net change in nesting level is 0" in {
    val applyIn = Fn("x", Ref(0))
    val applyTo = Ap(Ref(0), Ref(1))
    val input = Ap(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (applyTo)
  }

  // (\x.\y.x) (<0> <1>) is like....
  //     => \A.\B.((\x.\y.x) (B A))
  //     => \A.\B.((\x.\y.<1>) (<0> <1>) )
  //     => \A.\B.\y.(<1> <2>)
  //     => \A.\B.\y.(B A)
  //     => indexes shifted by one, the same amount as the BoundIdx value in \x.\y 
  it should
    "increment the higher-level BindingRefs if the net change in nesting level is 1" in {
    val applyIn = Func(Binding("x"), Func(Binding("y"), BindingRef(1)))
    val applyTo = App(BindingRef(0), BindingRef(1))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input) tap println
    result should be (Func(Binding("y"), App(BindingRef(1), BindingRef(2))))
  }

  // (\x.\y.\z.x) (<0> <1>) is like....
  //     => \A.\B.((\x.\y.\z.x) (B A))
  //     => \A.\B.((\x.\y.\z.<2>) (<0> <1>))
  //     => \A.\B.\y.\z.(<2> <3>)
  //     => \A.\B.\y.\z.(B A)
  //     => indexes shifted by two, the same amount as the BoundIdx value in \x.\y.\z
  it should
    "increment the higher-level BindingRefs if the net change in nesting level is 2" in {
    val applyIn = Func(Binding("x"), Func(Binding("y"), Func(Binding("z"), BindingRef(2))))
    val applyTo = App(BindingRef(0), BindingRef(1))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (Func(Binding("y"), Func(Binding("z"), App(BindingRef(2), BindingRef(3)))))
  }

  // (\x.x) (\y.<1>) is like....
  //     => \A.((\x.x) (\y.A))
  //     => \A.((\x.<0>) (\y.<1>)) 
  //     => \A.\y.<1>
  //     => \A.\y.A
  //     => indexes did not shift because the target in \x was at the root of \x, even when nested deeply in y
  it should
    "not increment higher-level BindingRefs if the net change in nesting level is 0, even when right-operand bindings are nested" in {
    val applyIn = Func(Binding("x"), BindingRef(0))
    val applyTo = Func(Binding("y"), BindingRef(1))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (Func(Binding("y"), BindingRef(1)))
  }

  // (\x.\y.x) (\z.<1>) is like....
  //     => \A.((\x.\y.x) (\z.A))
  //     => \A.((\x.\y.<1>) (\z.<1>)) 
  //     => \A.\y.\z.<2>
  //     => \A.\y.A
  //     => indexes shift by the sum of the target & destination bindingRefs (for external bindings to right operand)
  it should
    "increment higher-level BindingRefs if the net change in nesting level is 1, even when right-operand bindings are nested" in {
    val applyIn = Fn("x", Fn("y", Ref(1)))
    val applyTo = Fn("z", Ref(1))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (Fn("y", Fn("z", Ref(2))))
  }

  // (\x.x) (\y.<0>) is like....
  //     => \A.((\x.x) (\y.y))
  //     => \A.((\x.<0>) (\y.<0>)) 
  //     => \A.\y.<0>
  //     => \A.\y.y
  //     => indexes did not shift because the right operand bindings are self contained
  it should
    "not increment self-contained BindingRefs if the net change in nesting level is 0" in {
    val applyIn = Fn("x", Ref(0))
    val applyTo = Fn("y", Ref(0))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (Fn("y", Ref(0)))
  }

  // (\x.\y.x) (\m.\n.(<0> <1> <2>)) is like....
  //     => \A.((\x.\y.x) (\m.\n.(n m A)))
  //     => \A.((\x.\y.<1>) (\m.\n.(<0> <1> <2>)))
  //     => \A.\y.\m.\n.(<0> <1> <3>)
  //     => \A.\y.\m.\n.(n m A)
  //     => indexes only shifted for indexes not contained in right operand (A shifted, m and n did not)
  it should
    "increment only the BindingRefs that are not fully contained by the right operand" in {
    val applyIn = Func(Binding("x"), Func(Binding("y"), BindingRef(1)))
    val applyTo = Func(Binding("m"), Func(Binding("n"), App(App(BindingRef(0), BindingRef(1)), BindingRef(2))))
    val input = App(applyIn, applyTo)

    val result = betaReduce(input)
    result should be (Func(Binding("y"), Func(Binding("m"), Func(Binding("n"), App(App(BindingRef(0), BindingRef(1)), BindingRef(3))))))
  }

  //(λx y z.x y z) (λx y.x y)
  //(λx y z.2 1 0) (λx y.1 0)
  //(λy z.(λx y.1 0) 1 0) 
  //(λy z.(λy.2 0) 0) 
  //(λy z.(λ1 0)) 
  "n" should "n" in {

    val applyL = Fn("x", Fn("y", Fn("z", Ap(Ap(Ref(2), Ref(1)), Ref(0)))))
    val applyR = Fn("x", Fn("y", Ap(Ref(1), Ref(0))))
    val input = App(applyL, applyR)

    input should be {
      App(Func(Binding("x"),Func(Binding("y"),Func(Binding("z"),App(App(BindingRef(2),BindingRef(1)),BindingRef(0))))),
        Func(Binding("x"),Func(Binding("y"),App(BindingRef(1),BindingRef(0)))))
    }

    val result = betaReduce(input)
    result should be (Fn("y", Fn("z", Ap(Ref(1), Ref(0)))))
  }
  "(λa b c.<2> <1> <5>) <2>" should "reduce to λb c.<4> <1> <4>" in {

    val applyL = Fn("a", Fn("b", Fn("c", Ap(Ap(Ref(2), Ref(1)), Ref(5)))))
    val applyR = Ref(2)
    val input = App(applyL, applyR)

    val result = betaReduce(input)
    result should be (Fn("b", Fn("c", Ap(Ap(Ref(4), Ref(1)), Ref(4)))))
  }
  
  "(λa b c.<2> <1> <5>) (<2> <3>)" should "reduce to λb c.<4> <5> <1> <4>" in {

    val applyL = Fn("a", Fn("b", Fn("c", Ap(Ap(Ref(2), Ref(1)), Ref(5)))))
    val applyR = Ap(Ref(2), Ref(3))
    val input = Ap(applyL, applyR)

    val result = betaReduce(input)
    result should be (Fn("b", Fn("c", Ap(Ap(Ap(Ref(4), Ref(5)), Ref(1)), Ref(4)))))
  }
  
  "(λx.(λy.<1> <0>)) <1>" should "reduce to (λy.<2> <0>)" in {
    val applyL = Fn("x", Fn("y", Ap(Ref(1), Ref(0))))
    val applyR = Ref(1)
    val input = App(applyL, applyR)

    val result = betaReduce(input)
    result should be (Fn("y", Ap(Ref(2), Ref(0))))
  }
}

class LambdaAdjustBindingsTests extends AnyFlatSpec {
  import types.Expr
  import types.Expr._
  import LcParser.{ ParsedExpr => PExpr }

  "a" should "a" in {
    betaReduce {
      Func(Binding("x"), BindingRef(0))
    } should be {
      Func(Binding("x"), BindingRef(0))
    }
  }

  "b" should "b" in {
    betaReduce {
      Func(Binding("x"), Func(Binding("y"), BindingRef(1)))
    } should be {
      Func(Binding("x"), Func(Binding("y"), BindingRef(1)))
    }
  }

  "c" should "c" in {
    betaReduce {
      Func(Binding("x"), Func(Binding("y"), BindingRef(0)))
    } should be {
      Func(Binding("x"), Func(Binding("y"), BindingRef(0)))
    }
  }

  "d" should "d" in {
    betaReduce {
      Func(Binding("y"), App(FreeVar("y"), BindingRef(0))) 
    } should be {
      Func(Binding("y'"), App(FreeVar("y"), BindingRef(0)))
    }
  }

  "e" should "e" in {
    betaReduce {
      App(
        Func(Binding("n"), Func(Binding("y'"), Func(Binding("y"), App(BindingRef(2), BindingRef(0))))),
        FreeVar("y")
      ) 
    } should be {
      Func(Binding("y'"), Func(Binding("y''"), App(FreeVar("y"), BindingRef(0))))
    }
  }

  "f" should "f" in {
      def parseAndReduce(label: String)(input: String)(expected: String): Either[String, PExpr] = for {
        parsed <- LcParser.parse(input)
        betaReduced = betaReduce(parsed)
        etaReduced = etaReduce(parsed)
        noIdx = fromIdx(etaReduced)
        success = asStr(noIdx) == expected 
        _ = println(s"[$success] $label ==> $input | ${asStr(parsed)} | ${asStr(betaReduced)} | ${asStr(noIdx)} == $expected")
      } yield noIdx 

      parseAndReduce("beta reduce 1")("(λx.x) y")("y")
      parseAndReduce("beta reduce 2")("(λx.x x x) (λy.y)")("λy.y")
      parseAndReduce("beta reduce 3")("(λx y.x y) (λy.y)")("λy.y")
      parseAndReduce("beta reduce 4")("(λx y z.x y z) (λx y.x y) (λx.x) x")("x")
      parseAndReduce("beta reduce 5")("(λx y z.x y z) (λy x.x z y) (λx.x) x")("x x")
      parseAndReduce("beta reduce 6")("x z (λx.x)")("x z (λx.x)")
      parseAndReduce("beta reduce 7")("λx.y x")("y")
      parseAndReduce("beta reduce 8")("(λx.(λy.x) y) z")("z")
      parseAndReduce("alpha convert 1")("(λx y.x y) y")("λy.y y") 

  }
}