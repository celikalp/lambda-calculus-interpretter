package lambda

import lambda.evaluation.{AlphaRenaming, Evaluation, EvaluationStrategy, Substitution}

/**
  * Abstract Syntax Tree/Model
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

sealed abstract class Term {
  def getFreeVars : Set[String]
  def getBoundVars : Set[String]
  def evaluate : Term = (new Evaluation)(this)
  def evaluate(evalStrategy : EvaluationStrategy) : Term = (new Evaluation)(this)
  def substitute(substituted : String, substituent : Term) : Term = new Substitution(substituted, substituent)(this)
  def alphaRename(varNamesToChange : Set[String]) : Term = new AlphaRenaming(varNamesToChange)(this)
}
case class Var(name: String) extends Term {
  def getBoundVars : Set[String] = Set()
  def getFreeVars : Set[String] = Set(name)
}
case class Fun(arg: String, body: Term) extends Term {
  def getBoundVars : Set[String] = Set(arg) ++ body.getBoundVars
  def getFreeVars : Set[String] = {
    val vs = body.getFreeVars
    vs filterNot (x => (x == arg))
  }
}
case class FApp(f: Term, v: Term) extends Term {
  def getBoundVars : Set[String] = f.getBoundVars ++ v.getBoundVars
  def getFreeVars : Set[String] = f.getFreeVars ++ v.getFreeVars
}
case class Let(n: String, t1: Term, t2: Term) extends Term {
  def getBoundVars : Set[String] = Set(n) ++ t2.getBoundVars
  def getFreeVars : Set[String] = {
    val vs = t1.getFreeVars
    (vs filterNot (x => (x == n))) ++ t2.getFreeVars
  }
}
