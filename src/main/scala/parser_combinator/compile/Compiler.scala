package parser_combinator.compile

import org.slf4j.LoggerFactory
import parser_combinator.model.{AST, CompilationError}

object Compiler {
  private lazy val LOGGER = LoggerFactory.getLogger(this.getClass)
  def screen(x: Any): Unit =
    pprint.pprintln(x, width = 120, height = 1000)

  val debug  = true
  val lexer  = new Lexer
  val parser = new ParserTop

  def parseFunction(code: String) : Either[CompilationError, AST] = {
    if (debug) LOGGER.info(s"Parsing Code: \n$code")

    val x = for {
      tokens ← lexer(code).right
      ast    ← parser(tokens).right
    } yield (tokens, ast)

    if (debug && x.isRight) {
      val tokens = x.right.get._1
      val ast = x.right.get._2
      screen(tokens)
      screen(ast)
    }

    x.map{ case (tokens, ast) ⇒ ast }
  }
}
