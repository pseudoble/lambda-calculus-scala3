package io.pseudoble.lambda

import scala.util.chaining.scalaUtilChainingOps

import io.pseudoble.effects.typeclasses._
import io.pseudoble.effects.typeclasses.Extensions._
import io.pseudoble.parser.Parser
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.lambda.types.Expr

// import scala.util.matching.Regex
// import io.pseudoble.tools._

object LcParser {
  var appNum = 0
  enum ParsedExpr:
    case Literal(value: Boolean | Int | Double | BigDecimal | String)
    case Var(name: String)
    case Func(binding: Var, expression: ParsedExpr)
    case App(left: ParsedExpr, right: ParsedExpr)

  private def parseBool: Parser[ParsedExpr.Literal] = withLogging(s"Bool") {
    (stringP("true") <|> stringP("false")) map (v => ParsedExpr.Literal.apply(v.toBoolean))
  }

  private def parseDouble: Parser[ParsedExpr.Literal] = withLogging(s"Double") {
    regexP("^\\d+\\.\\d+".r) map (v => ParsedExpr.Literal.apply(v.toDouble))
  }

  //  private def parseString: Parser[ParsedExpr.Literal] = ???

  private def parseInt: Parser[ParsedExpr.Literal] = withLogging(s"Int") {
    regexP("^\\d+".r) map (v => ParsedExpr.Literal.apply(v.toInt))
  }

  private def parseConst: Parser[ParsedExpr.Literal] = withLogging(s"Const") {
    parseBool <|> parseDouble <|> parseInt /* <|> parseString */
  }

  private def parseVar: Parser[ParsedExpr.Var] = withLogging(s"Var") {
    regexP("^[A-Za-z_]\\w*".r) map ParsedExpr.Var.apply
  }

  private def parseApply: Parser[ParsedExpr.App] = {
    withLogging(s"Apply") { 
      for {
        // Expr
        head <- withLogging(s"Apply LEFT")(parseFunc <|> parseConst <|> parseVar <|> parseParens) 
        // Vector[Expr]
        tail <- withLogging(s"Apply RIGHT(S)")(atLeast(1)(wsP :*> (parseFunc <|> parseConst <|> parseVar <|> parseParens)))
        fst = ParsedExpr.App.apply(head, tail.head): ParsedExpr.App
        x = tail.tail match {
          case Nil => fst
          case items => items.foldLeft(fst)(ParsedExpr.App.apply(_, _)) 
        }
      } yield x
//      for {
//        left <- withLogging(s"Apply LEFT")(parseParens <|> parseFunc <|> parseConst <|> parseVar)
//        _ <- withLogging(s"Apply WS")(wsP)
//        right <- withLogging(s"Apply RIGHT")(parseParens <|> parseFunc <|> parseConst <|> parseVar)
//      } yield ParsedExpr.App.apply(left, right)
    }
  }

  private def parseFunc: Parser[ParsedExpr.Func] = {
    for {
      _ <- (charP('λ') <|> charP('\\')) :*> opt(wsP)
      bindings <- (parseVar * many(wsP :*> parseVar)) map ((h,t) => h +: t)
      _ <- opt(wsP) :*> charP('.') :*> opt(wsP)
      expr <- parseExpr 
      coreFunc = ParsedExpr.Func(bindings.last, expr): ParsedExpr.Func
      resultExpr = bindings.dropRight(1) match {
        case IndexedSeq() => coreFunc // if there was only one var, we're done
        case items => items.foldRight(coreFunc)(ParsedExpr.Func(_, _)) // if there are more vars, foldRight on them to create the nested data structure
      }
    } yield resultExpr
  }

  private def parseParens: Parser[ParsedExpr] = withLogging(s"Parens") {
    charP('(') :*> parseExpr <*: charP(')')
  }

  private def parseExpr: Parser[ParsedExpr] = for {
    _ <- opt(wsP) 
    expr <- parseFunc <|> parseApply <|> parseConst <|> parseVar <|> parseParens
    _ <- opt(wsP) 
  } yield expr
  
  def parse(stream: String): Either[String, Expr] = {
    appNum = 0
    parseExpr.map(toIdx).parse(stream) match {
      case Some(("", result)) => Right(result)
      case Some((unparsed, result)) => Left(s"Invalid expression: '$stream' was not fully parsed. '$unparsed' remained.")
      case None => Left(s"Invalid expression: Parser could not parse '$stream'")
    }
  }
  
  private def withLogging[T](label: String)(subject: Parser[T]): Parser[T] = Parser { stream =>
    val logId = appNum
    appNum += 1
    val indent = "-"*appNum
    //println(s"$indent[$logId] $label: parsing '$stream'...")
    subject.parse(stream) tap { x => 
      x match {
        case Some((_, out)) => //println(s"$indent[$logId] $label: *** parsing '$stream' resulted in '$out' ***")
        case None => //println(s"$indent[$logId] $label: parsing '$stream' found no match")
      }
      appNum += -1 
    }
    
  }
}


object ExprParser {
  import io.pseudoble.effects.instances._
  
  // currying for type arguments!
  // this is similar currying for function arguments:
  //    val add = (l: Int) => (r: Int) => l + r
  type EitherC = [L] =>> [R] =>> Either[L, R]
  type StateC = [S] =>> [A] =>> State[S ,A]
  
  // Composing types Either and State into an Either[.., State[.., A]]
  // into a new type that can take the error type and state types up front.
  // The resulting output type asks only for A, making it usable as an 
  // effect type!
  //  e.g.    type EitherStrStateInt[A] = ESC[String, Int][A] = Either[String, State[Int, A]]
  type ESC = [L, S] =>> [A] =>> Either[L, State[S, A]]
  
  // Same as above, but now Either and State are abstracted out. This now composes any two
  // higher kinded types into a new higher kinded type that can function as an effect type.
  type FG = [F[_]] =>> [G[_]] =>> [A] =>> F[G[A]]

  // but does scala like that? Yes! Yes it does.  
  def t1[F[_], G[_]](f1: Functor[F])(f2: Functor[G]): Functor[FG[F][G]] = ???

  // Ok, so lets implement a simple example of Functor to test this with
  trait Functor2[F[_]]:
    def fmap[A,B](fa: F[A])(f: A => B): F[B]

  // We'll get a simple Functor2 type class instance for Option, Either, and State
  given Functor2[Option] with {
    def fmap[A,B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  given [L]: Functor2[EitherC[L]] with {
    def fmap[A,B](fa: Either[L, A])(f: A => B): Either[L, B] = fa.map(f)
  }

  given [S]: Functor2[StateC[S]] with {
    def fmap[A,B](fa: State[S, A])(f: A => B): State[S, B] = fa.map(f)
  }

  // Same as t1 above, but with the new Functor2 type. It works too, no surprises here. Just
  // sanity checking...
  def compose1[F[_], G[_]](f1: Functor2[F])(f2: Functor2[G]): Functor2[FG[F][G]] = ???
  
  // Ok, Lets actually implement compose now! This takes two Functor type class instances and
  // composes them. F[_] + G[_] = F[G[_]]
  // in other words, given fga: F[G[A]], we could:
  //    functorF.fmap(fga)(ga => functorG.fmap(ga)(f))
  // but that's noisy and hard to underestand.
  // compose2, here, enables you to create a new Functor2 that does this for you, nice and quiet!
  //    functorFG.fmap(fga)(f)
  def compose2[F[_], G[_]](f: Functor2[F], g: Functor2[G]): Functor2[FG[F][G]] = new Functor2[FG[F][G]] {
    def fmap[A,B](fga: FG[F][G][A])(m: A => B): FG[F][G][B] = f.fmap(fga)(ga => g.fmap(ga)(m))
  }
  
  // Lets put this to the test.
  // @main 
  def tester = {


    // gimme the type class instances for state and either
    val g = summon[Functor2[StateC[Int]]]
    val f = summon[Functor2[EitherC[String]]]

    // now compose them into a new type class instance in the form of Either[State[_]]    
    val composed: Functor2[FG[EitherC[String]][StateC[Int]]] = compose2(f, g)

    // setup an input in the right shape
    val input: Either[String, State[Int, Int]] = Right(State(s => (s * 10, 1)))
    
    // now map using our composed functor type class instance!
    val output = composed.fmap(input)(_ + 10)

    // did it work?    
    (input, output) match {
      case (Right(i), Right(o)) => 
        val ir = i.run(5)
        val or = o.run(5)
        // Yes, ir should be (50, 1) and or should be (50,11). our composed functor type class instance
        // successfully fmapped into the either and then into the state to modify our internal value
        // as it should. The state was preserved!
        println(s"$ir => $or") 
    }
  }
}