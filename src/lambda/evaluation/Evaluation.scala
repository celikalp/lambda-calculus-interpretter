package lambda.evaluation

import lambda._

/**
  * Evaluation class defines reduction methods
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

class Evaluation {
  def apply(exp : Term) : Term = useFullBetaReduction(exp)
  def apply(exp : Term)(evalStrategy : EvaluationStrategy) : Term = evalStrategy match {
    case FULL_BETA => useFullBetaReduction(exp)
    case NORMAL_ORDER => useNormalOrderReduction(exp)
    case CALL_BY_VALUE => useCallByValueReduction(exp)
    case CALL_BY_NAME => useCallByNameReduction(exp)
  }

  def useFullBetaReduction(exp : Term) : Term = exp match {
    case FApp(Fun(arg, body), t2) => new Substitution(arg, t2)(body)
    case FApp(t1, t2) => {
      val l = FApp(apply(t1), t2)
      if(l != this) l
      else FApp(t1, apply(t2))
    }
    case Fun(arg, body) => Fun(arg, apply(body))
    case Let(v,t1,t2) => new Substitution(v, t1)(t2)
    case _ => exp
  }
  def useNormalOrderReduction(exp : Term) : Term = {exp}
  def useCallByValueReduction(exp : Term) : Term = {exp}
  def useCallByNameReduction(exp : Term) : Term = {exp}
}
