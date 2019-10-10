package parser_combinator.model

import scala.util.parsing.input.Positional

//
// TOKEN DEFINITIONS
//
sealed trait Token extends Positional
sealed trait LITERAL extends Token
case class INT_LITERAL(int: Int) extends LITERAL
case class IDENTIFIER(str: String) extends Token
case class BIN_OP1(str: String) extends Token
case class BIN_OP2(str: String) extends Token
case class BIN_OP3(str: String) extends Token
case class BIN_OP4(str: String) extends Token
case class MINUS() extends Token
case class INTEGER() extends Token
case class IF() extends Token
case class ELSE() extends Token
case class OPEN_CURLY() extends Token
case class CLOSE_CURLY() extends Token
case class OPEN_PAREN() extends Token
case class CLOSE_PAREN() extends Token
case class USER_FUNCTION() extends Token
case class RETURN() extends Token
case class EQUAL() extends Token
case class DEF() extends Token
case class LET() extends Token
case class COMMENT() extends Token
case class COLON() extends Token
case class SEMI() extends Token
case class COMMA() extends Token
