package lambda

import scala.util.parsing.combinator.lexical.StdLexical

/**
  * Definition of lambda term lexer
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

class LambdaLexer extends StdLexical{
  delimiters ++= Seq("\\", "λ", ".", "(", ")", "=")
  reserved ++= Seq("let", "in")
  override def letter = elem("letter", c => c.isLetter && c != 'λ')
}
