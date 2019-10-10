package parser_combinator.compile

import parser_combinator.model._

class ParserExpression extends ParserBase {

  lazy val expression4: PackratParser[Expression] = new Expressions(Some(expression3)).expression

  private lazy val expression3: PackratParser[Expression] = new Expressions(Some(expression2)) {
    override lazy val binOpExpression: PackratParser[BinOpExpression] = {
      expression ~ operator3 ~ expression2 ^^ {
        case (expr1: Expression) ~ BIN_OP3(op) ~ (expr2: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: binOpExpression $expr1, $op, $expr2")
          BinOpExpression(op, expr1, expr2)
      }
    }
  }.expression

  private lazy val expression2: PackratParser[Expression] = new Expressions(Some(expression1)) {
    override lazy val binOpExpression: PackratParser[BinOpExpression] = {
      expression ~ operator2 ~ expression1 ^^ {
        case (expr1: Expression) ~ BIN_OP2(op) ~ (expr2: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: binOpExpression $expr1, $op, $expr2")
          BinOpExpression(op, expr1, expr2)
      }
    }
    lazy val binAmbOpExpression: PackratParser[BinOpExpression] = {
      expression ~ ambiguous_op ~ expression1 ^^ {
        case (expr1: Expression) ~ BIN_OP2(op) ~ (expr2: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: binOpExpression $expr1, $op, $expr2")
          BinOpExpression(op, expr1, expr2)
      }
    }
    override def parsers: List[(String, PackratParser[Expression])] =
      addBefore("binAmbOpExpression" → binAmbOpExpression, super.parsers, "conditionExpression")
  }.expression


  private lazy val expression1: PackratParser[Expression] = new Expressions(None) {
    override lazy val binOpExpression: PackratParser[BinOpExpression] = {
      expression ~ operator1 ~ expression ^^ {
        case (expr1: Expression) ~ BIN_OP1(op) ~ (expr2: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: binOpExpression $expr1, $op, $expr2")
          BinOpExpression(op, expr1, expr2)
      }
    }
    lazy val minusUnaryOpExpression: PackratParser[UnOpExpression] = {
      ambiguous_op ~ expression ^^ {
        case BIN_OP2(op) ~ (expr1: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: unOpExpression $op, $expr1")
          UnOpExpression(op, expr1)
      }
    }
    lazy val simpleLitExpression: PackratParser[SimpleExpression] = {
      literal ^^ {
        case INT_LITERAL(value) ⇒
          if (debugMatch) logger.info(s"PARSE:TERM: IntegerLiteralTerm $value")
          SimpleExpression(lit = Some(IntLiteralTerm(value)))
      }
    }
    lazy val simpleVarExpression: PackratParser[SimpleExpression] = {
      variable ^^ { term: VariableTerm ⇒
        SimpleExpression(name = Some(term))
      }
    }
    lazy val wrappedExpression: PackratParser[Expression] = {
      OPEN_PAREN() ~ ParserExpression.this.expression4 ~ CLOSE_PAREN() ^^ {
        case _ ~ (expr: Expression) ~ _ ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: wrappedExpression $expr")
          expr
      }
    }
    lazy val innerExpression: PackratParser[Expression] = {
      ParserExpression.this.expression4 ^^ { e: Expression ⇒
        e
      }
    }

    override def parsers: List[(String, PackratParser[Expression])] = super.parsers ::: List(
      "minusUnaryOpExpression" → minusUnaryOpExpression,
      "simpleLitExpression" → simpleLitExpression,
      "simpleVarExpression" → simpleVarExpression,
      "wrappedExpression" → wrappedExpression,
      "innerExpression" → innerExpression
    )
  }.expression

  /**
   * Defines a base class for expressions. All the parsers inside the base class are inherited by the anonymous
   * classes - expressions above.
   *
   * @param previousExpressions Next lower level of expressions
   */
  private class Expressions(val previousExpressions: Option[PackratParser[Expression]]) {
    lazy val expression: PackratParser[Expression] = generateParser

    lazy val ifElseExpression: PackratParser[IfElseExpression] = positioned {
      IF() ~ OPEN_PAREN() ~ expression ~ CLOSE_PAREN() ~ expression ~ ELSE() ~ expression ^^ {
        case _ ~ _ ~ (expr1: Expression) ~ _ ~ (expr2: Expression) ~ _ ~ (expr3: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: IfElseExpression $expr1, $expr2, $expr3")
          IfElseExpression(expr1, expr2, expr3)
      }
    }

    lazy val binOpExpression: PackratParser[BinOpExpression] = positioned {
      expression ~ operator4 ~ expression3 ^^ {
        case (expr1: Expression) ~ BIN_OP4(op) ~ (expr2: Expression) ⇒
          if (debugMatch) logger.info(s"PARSE:EXPRESSION: binOpExpression $expr1, $op, $expr2")
          BinOpExpression(op, expr1, expr2)
      }
    }

    /**
     * @return List of all the parsers on the current expressions level
     */
    def parsers: List[(String, PackratParser[Expression])] = {
      val a = List(
        "binOpExpression" → binOpExpression,
        "ifElseExpression" → ifElseExpression,
      ) ::: previousExpressions.map(e ⇒ ("previousExpression" → e) :: Nil).getOrElse(Nil)
      a
    }

    /**
     * Generate a parser by taking an alternation of all the parsers inside the [[parsers]] list.
     *
     * @return alternation of all the parsers inside the [[parsers]] list
     */
    private def generateParser: PackratParser[Expression] = positioned {
      val head = dbg(parsers.head._2)(name = parsers.head._1)
      parsers.tail.foldLeft(head) {
        case (previous, (expressionName, expression)) ⇒ previous | dbg(expression)(name = expressionName)
      }
    }

    /**
     * Adds the parser before the specified parser to the list.
     *
     * @param parser Parser to add
     * @param parsers List of parsers
     * @param before Name of the existing parser in the list to add the new parser before
     * @return List of parsers with the new parser included
     */
    protected def addBefore(parser:  (String, PackratParser[Expression]),
                            parsers: List[(String, PackratParser[Expression])],
                            before:  String): List[(String, PackratParser[Expression])] =
      parsers.takeWhile(_._1 != "conditionExpression") :::
        List(parser) :::
        parsers.dropWhile(_._1 != "conditionExpression")
  }
}
