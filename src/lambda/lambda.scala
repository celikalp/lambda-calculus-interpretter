
/*
 * @author: Ahmet Alparslan Celik
 *
*/

import LambdaCalculusParsing._

package LambdaCalculusParsing {
  import scala.util.parsing.combinator._
  import scala.util.parsing.combinator.lexical.StdLexical
  import scala.util.parsing.combinator.syntactical.StdTokenParsers

  // === Abstract Syntax Model ===
  abstract class Term {

    // Reduction
    def reduction : Term = this match {
      case FApp(Fun(arg, body), t2) => body.substitute(arg, t2)
      case FApp(t1, t2) => {
        val l = FApp(t1.reduction, t2)
        if(l != this) l
        else FApp(t1, t2.reduction)
      }
      case Fun(arg, body) => Fun(arg, body.reduction)
      case Let(v,t1,t2) => t2.substitute(v,t1)
      case _ => this
    }

    def substitute(substituted : String, substituent : Term) : Term = this match {
      case Var(name) => if(name.equals(substituted)) substituent else this
      case Fun(arg, body) => {
        if (!arg.equals(substituted)){
          if(substituent.getFreeVars contains arg)
            (this alphaRenaming substituent.getFreeVars).substitute(substituted, substituent)
          else
            Fun(arg, body.substitute(substituted, substituent))
        } else
          this
      }
      case FApp(t1, t2) => FApp(t1.substitute(substituted, substituent), t2.substitute(substituted, substituent))
      case Let(v,t1,t2) => {
        if (!v.equals(substituted)){
          if(substituent.getFreeVars contains v)
            (this alphaRenaming substituent.getFreeVars).substitute(substituted, substituent)
          else
            Let(v,t1,t2.substitute(substituted, substituent))
        } else
          this
      }
    }

    // Alpha renaming
    def alphaRenaming(varsNamesToChange : Set[String]) : Term = this match {
      case Var(_) => this
      case Fun(arg, body) => {
        if (varsNamesToChange contains arg) {
          val newArg = getUniqueVarName(arg, varsNamesToChange ++ getBoundVars)
          Fun(newArg, body.substitute(arg, Var(newArg)))
        } else
          Fun(arg, body alphaRenaming varsNamesToChange)
      }
      case FApp(t1, t2) => FApp(t1 alphaRenaming varsNamesToChange, t2 alphaRenaming varsNamesToChange)
      case Let(v,t1,t2) => {
        if (varsNamesToChange contains v) {
          val newV = getUniqueVarName(v, varsNamesToChange ++ getBoundVars)
          Let(newV, t1, t2.substitute(v, Var(newV)))
        } else
          Let(v,t1,t2 alphaRenaming varsNamesToChange)
      }
    }

    // Add prime char
    def getUniqueVarName(name : String, varNamesUsed : Set[String]) : String = {
      val newName = name + "'"
      if(varNamesUsed contains newName)
        getUniqueVarName(newName, varNamesUsed)
      else
        newName
    }

    // Return fee variables
    def getFreeVars : Set[String] = this match {
      case Var(n) => Set(n)
      case Fun(arg, body) => {
        val vs = body.getFreeVars
        vs filterNot (x => (x == arg))
      }
      case FApp(t1, t2) => t1.getFreeVars ++ t2.getFreeVars
      case Let(v, t1, t2) => {
        val vs = t1.getFreeVars
        (vs filterNot (x => (x == v))) ++ t2.getFreeVars
      }
    }

    // Return bounded variables
    def getBoundVars : Set[String] = this match {
      case Var(n) => Set()
      case Fun(arg, body) => Set(arg) ++ body.getBoundVars
      case FApp(t1, t2) => t1.getBoundVars ++ t2.getBoundVars
      case Let(v,t1,t2) => Set(v) ++ t2.getBoundVars
    }
  }
  case class Id(name:String) extends Term {
    override def toString = name
    //override def toString = "\"" + name + "\""
  }
  case class Var(name: String) extends Term
  case class Fun(arg: String, body: Term) extends Term
  case class FApp(f: Term, v: Term) extends Term
  case class Let(n: String, t1: Term, t2: Term) extends Term

  // === Lambda Lexer ===
  class LambdaLexer extends StdLexical{
    delimiters ++= Seq("\\", "λ", ".", "(", ")", "=")
    reserved ++= Seq("let", "in")
    override def letter = elem("letter", c => c.isLetter && c != 'λ')
  }

  // === Lambda Parser ===
  class LambdaParser extends StdTokenParsers with PackratParsers {
    type Tokens = StdLexical
    val lexical = new LambdaLexer

    // = Grammer for lambda calculus =
    type P[+A] = PackratParser[A]
    lazy val lam_expr : P[Term] = application | non_application
    lazy val non_application : P[Term] = lambda | let | variable | identifier | parens
    lazy val lambda: P[Term] = ("\\" | "λ") ~> rep1(identifier) ~ "." ~ lam_expr ^^ { case i ~ "." ~ e => (i :\ e)((x,y) => Fun(x.toString, y)) }
    lazy val let : P[Let] = "let" ~ identifier ~ "=" ~ lam_expr ~ "in" ~ lam_expr ^^ { case "let" ~ i ~ "=" ~ l_e ~ "in" ~ r_e => Let(i.toString, l_e, r_e) }
    lazy val parens : P[Term] = "(" ~> lam_expr <~ ")"
    lazy val application : P[Term] = lam_expr ~ rep1(lam_expr) ^^ { case l_e ~ r_e => (l_e /: r_e)(FApp(_, _)) }
    lazy val identifier : P[Id] = ident ^^ { Id(_) }
    lazy val variable : P[Var] = ident ^^ { Var(_) }

    def parse(t: String): ParseResult[Term] = {
      val tokens = new lexical.Scanner(t)
      phrase(lam_expr)(tokens)
    }
  }

  // === Pretty Printer ===
  class LambdaPrettyPrinter {
    def apply(lam_expr : Term): String =
      lam_expr match {
        case Var(name)        => name
        case Fun(arg, body)   => "λ" + arg + "." + apply(body)
        case FApp(f, v)       => {
          val l = f match {
            case Fun(_,_) => s"(${apply(f)})"
            case _ => apply(f)
          }
          val r = v match {
            case Var(_) => apply(v)
            case _ => s"${apply(v)}"
          }
          s"$l $r"
        }
        case Let(n, t1, t2)   => "let " + n + "=" + apply(t1) + " in " + apply(t2)
      }
  }
}

object main extends App {
  val parser = new LambdaParser
  val pretty = new LambdaPrettyPrinter

  import parser._

  def parse_lambda(x:String):Option[Term] =
    parse(x) match {
      case Success(expr, _) => Some(expr)
      case NoSuccess(f_msg, _) => None
    }

  def eval_to_value(x:Term):Term =
    x.reduction

}
