package parser_combinator.compile

import parser_combinator.model._
import scala.util.parsing.combinator.RegexParsers

//
// LEXER
//
class Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace     = "[ \t\r\f\n]+".r
  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg,  next) ⇒ Left(LexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _)    ⇒ Right(result)
    }
  }
  def tokens: Parser[List[Token]] = {
    phrase(rep1(let
      | function | theReturn | theIf | theElse | theDef
      | equal | colon | comma | semi | integer
      | oParen | cParen | oCurly | cCurly
      | operator1 | operator2 | operator3 | operator4
      | literal | identifier
    )) ^^ { rawTokens => rawTokens.filter(_ != COMMENT()) }
  }

  def identifier: Parser[IDENTIFIER] = positioned { "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) } }
  def literal   : Parser[LITERAL]    = positioned { """[0-9]+""".r             ^^ { dub ⇒ INT_LITERAL(dub.toInt) } }

  def operator1: Parser[BIN_OP1]     = positioned { """(\*|%|/)""".r           ^^ { x ⇒ BIN_OP1(x) } }
  def operator2: Parser[BIN_OP2]     = positioned { """(\+|>>|<<)""".r         ^^ { x ⇒ BIN_OP2(x) } }
  def operator3: Parser[BIN_OP3]     = positioned { """(>=|<=|<|>|!=|==)""".r  ^^ { x ⇒ BIN_OP3(x) } }
  def operator4: Parser[BIN_OP4]     = positioned { """(\|\||&&|and|or)""".r   ^^ { x ⇒ BIN_OP4(x) } }

  def integer:   Parser[INTEGER]       = positioned { ("integer" | "Int") ^^ (_ ⇒ INTEGER()) }
  def theIf:     Parser[IF]            = positioned { "if"                ^^ (_ ⇒ IF()) }
  def theElse:   Parser[ELSE]          = positioned { "else"              ^^ (_ ⇒ ELSE()) }
  def oCurly:    Parser[OPEN_CURLY]    = positioned { "{"                 ^^ (_ ⇒ OPEN_CURLY()) }
  def cCurly:    Parser[CLOSE_CURLY]   = positioned { "}"                 ^^ (_ ⇒ CLOSE_CURLY()) }
  def oParen:    Parser[OPEN_PAREN]    = positioned { "("                 ^^ (_ ⇒ OPEN_PAREN()) }
  def cParen:    Parser[CLOSE_PAREN]   = positioned { ")"                 ^^ (_ ⇒ CLOSE_PAREN()) }
  def function:  Parser[USER_FUNCTION] = positioned { "function"          ^^ (_ ⇒ USER_FUNCTION()) }
  def theReturn: Parser[RETURN]        = positioned { "return"            ^^ (_ ⇒ RETURN()) }
  def equal:     Parser[EQUAL]         = positioned { "="                 ^^ (_ ⇒ EQUAL()) }
  def colon:     Parser[COLON]         = positioned { ":"                 ^^ (_ ⇒ COLON()) }
  def semi:      Parser[SEMI]          = positioned { ";"                 ^^ (_ ⇒ SEMI()) }
  def comma:     Parser[COMMA]         = positioned { ","                 ^^ (_ ⇒ COMMA()) }
  def minus:     Parser[MINUS]         = positioned { "-"                 ^^ (_ ⇒ MINUS()) }
  def theDef:    Parser[DEF]           = positioned { "def"               ^^ (_ ⇒ DEF()) }
  def let:       Parser[LET]           = positioned { "let"               ^^ (_ ⇒ LET()) }

  def singleLineComment: Parser[COMMENT] = "//" ~ rep(not("\n") ~ ".".r) ^^^ COMMENT()
  def multiLineComment:  Parser[COMMENT] = "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ COMMENT()
}
