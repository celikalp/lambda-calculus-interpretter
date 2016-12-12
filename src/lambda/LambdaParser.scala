package lambda

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

/**
  * Context free grammar rules for lambda terms
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

class LambdaParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical
  val lexical = new LambdaLexer

  // = Grammer for lambda calculus =
  type P[+A] = PackratParser[A]
  lazy val lam_expr : P[Term] = application | non_application
  lazy val non_application : P[Term] = lambda | let | variable | parens
  lazy val lambda: P[Term] = ("\\" | "λ") ~> rep1(ident) ~ "." ~ lam_expr ^^ { case i ~ "." ~ e => (i :\ e)((x,y) => Fun(x, y)) }
  lazy val let : P[Let] = "let" ~ ident ~ "=" ~ lam_expr ~ "in" ~ lam_expr ^^ { case "let" ~ i ~ "=" ~ l_e ~ "in" ~ r_e => Let(i, l_e, r_e) }
  lazy val parens : P[Term] = "(" ~> lam_expr <~ ")"
  lazy val application : P[Term] = lam_expr ~ rep1(lam_expr) ^^ { case l_e ~ r_e => (l_e /: r_e)(FApp(_, _)) }
  lazy val variable : P[Var] = ident ^^ { Var(_) }

  def parse(t: String): ParseResult[Term] = {
    val tokens = new lexical.Scanner(t)
    phrase(lam_expr)(tokens)
  }
}
