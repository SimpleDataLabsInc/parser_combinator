package parser_combinator.compile

import parser_combinator.model._

import scala.util.parsing.combinator.{PackratParsers, Parsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}


class ParserBase extends Parsers with PackratParsers {
  import org.slf4j.LoggerFactory
  protected val logger = LoggerFactory.getLogger("Parser")
  protected val debugRules = true
  protected val debugMatch = false
  protected val tokenDebug = false

  //
  // TOKENS
  //
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    if (tokenDebug) logger.info(s"Tokens\n ${tokens.take(30)}")
    override def first: Token         = tokens.head
    override def atEnd: Boolean       = tokens.isEmpty
    override def pos:   Position      = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest:  Reader[Token] = new TokenReader(tokens.tail)
  }

  //
  // DEBUG
  //
  def dbg[T](p: => Parser[T])(name: String): Parser[T] = {
    if (!debugRules) p else
    name match {
      case "customFunction"     ⇒ log(p)(name)
      case "assignStatement"    ⇒ log(p)(name)
      case "returnStatement"    ⇒ log(p)(name)
      case "wrappedExpression"  ⇒ log(p)(name)
      case _ ⇒ p
    }
  }

  lazy val declaration: PackratParser[DeclarationTerm] = positioned {
    variable ~ COLON() ~ INTEGER() ^^ {
      case VariableTerm(name:String) ~ _ ~ _ ⇒ DeclarationTerm(name, "Integer")
    }
  }

  lazy val variable: PackratParser[VariableTerm] = positioned {
    identifier ^^ {
      case IDENTIFIER(x) ⇒ VariableTerm("%s".format(x))
    }
  }

  lazy val operator1: PackratParser[BIN_OP1] = positioned {
    accept("operator1", {
      case id @ BIN_OP1(name) =>
        if (debugMatch) logger.info(s"PARSE: operator1 $id")
        id
    })
  }

  lazy val operator2: PackratParser[BIN_OP2] = positioned {
    accept("operator2", {
      case id @ BIN_OP2(name) =>
        if (debugMatch) logger.info(s"PARSE: operator2 $id")
        id
    })
  }

  lazy val operator3: PackratParser[BIN_OP3] = positioned {
    accept("operator3", {
      case id @ BIN_OP3(name) =>
        if (debugMatch) logger.info(s"PARSE: operator3 $id")
        id
    })
  }

  lazy val operator4: PackratParser[BIN_OP4] = positioned {
    accept("operator4", {
      case id @ BIN_OP4(name) =>
        if (debugMatch) logger.info(s"PARSE: operator4 $id")
        id
    })
  }

  lazy val ambiguous_op: PackratParser[BIN_OP2] = positioned {
    MINUS() ^^ { _ ⇒
      BIN_OP2("-")
    }
  }

  lazy val identifier: PackratParser[IDENTIFIER] = positioned {
    accept("identifier", {
      case id @ IDENTIFIER(name) =>
        if (debugMatch) logger.info(s"PARSE: identifier $id")
        id
    })
  }

  lazy val literal: PackratParser[LITERAL] = positioned {
    accept(
      "literal", {
        case lit @ INT_LITERAL(name) ⇒
          if (debugMatch) logger.info(s"PARSE: literal int $lit")
          lit
      }
    )
  }

}
