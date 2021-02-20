package io.pseudoble.lambda

import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex
import io.pseudoble.effects.typeclasses._
import io.pseudoble.lambda.types.Expr
import io.pseudoble.parser.BasicParsers._
import io.pseudoble.parser.Parser
import io.pseudoble.tools._

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

  private def parseExpr: Parser[ParsedExpr] = withLogging(s"Expr") {
    opt(wsP) :*> (parseFunc <|> parseApply <|> parseConst <|> parseVar <|> parseParens) <*: opt(wsP)
  }
  
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
