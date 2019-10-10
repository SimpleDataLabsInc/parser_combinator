package parser_combinator.compile
import parser_combinator.model._

class ParserTop extends ParserExpression {

  def apply(tokens: Seq[Token]): Either[ParserError, AST] = {
    val reader = new PackratReader(new TokenReader(tokens))
    customFunction(reader) match {
      case NoSuccess(msg,  next) ⇒ Left(ParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _)    ⇒ Right(result)
    }
  }

  lazy val customFunction : PackratParser[FunctionDefinition] = positioned {
    val fn =
      DEF() ~ identifier ~ OPEN_PAREN() ~ rep(declaration ~ COMMA()) ~ rep(declaration)  ~ CLOSE_PAREN() ~
      COLON() ~ INTEGER() ~ EQUAL() ~
      OPEN_CURLY() ~ rep1(statement) ~ CLOSE_CURLY() ^^ {

      case _ ~ IDENTIFIER(funcName) ~ _ ~ args ~ lastArg ~ _ ~
        _ ~ _ ~ _ ~
        _ ~ (statements: List[Statement]) ~ _ ⇒

        if (debugMatch) logger.info(s"PARSE: Function: $funcName")
        FunctionDefinition(
          name = funcName,
          args = args.map(_._1) ++ lastArg,
          body = statements
        )
    }
    dbg(fn)(name = "customFunction")
  }

  lazy val statement: PackratParser[Statement] = positioned {
    val assignStatement = {
      LET() ~ variable ~ EQUAL() ~ expression4 ~ SEMI() ^^ {
        case _ ~ (variable: VariableTerm) ~ _ ~ (expression: Expression) ~ _ ⇒
          if (debugMatch) logger.info(s"PARSE: statement: Assign $variable = $expression")
          AssignStatement(variable, expression)
      }
    }
    val returnStatement = {
      RETURN() ~ expression4 ~ SEMI() ^^ {
        case _ ~ (expression: Expression) ~ _ ⇒
          if (debugMatch) logger.info(s"PARSE: statement: Return $expression")
          ReturnStatement(expression)
      }
    }
    dbg(assignStatement)(name = "assignStatement") |
      dbg(returnStatement)(name = "returnStatement")
  }
}
