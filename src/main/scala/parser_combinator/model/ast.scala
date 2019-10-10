package parser_combinator.model

import scala.util.parsing.input.Positional

object AST {
  protected var id = 0
  protected def getNextId: Int  = { id = id + 1; id }
  def resetId():           Unit = id = 0
}

trait AST extends Positional {
  import AST._
  protected var id: Int = getNextId
  def getId: Int = id
}

trait Term extends AST
trait LiteralTerm extends Term
case class VariableTerm(name: String) extends Term
case class DeclarationTerm(name: String, dataType: String) extends Term
case class IntLiteralTerm(value: Int) extends LiteralTerm

trait Expression extends AST
case class SimpleExpression(lit: Option[LiteralTerm] = None, name: Option[VariableTerm] = None) extends Expression
case class BinOpExpression(op: String, term1: Expression, term2: Expression) extends Expression
case class UnOpExpression(op: String, term1: Expression) extends Expression
case class IfElseExpression(expr1: Expression, expr2: Expression, expr3: Expression) extends Expression

trait Statement extends AST
case class AssignStatement(theVariable: VariableTerm, theExpression: Expression) extends Statement
case class ReturnStatement(theExpression: Expression) extends Statement

case class FunctionDefinition(name: String, args: List[DeclarationTerm], body: List[Statement]) extends AST
