package lambda.evaluation

import lambda._

/**
  * This class defines substitution
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

class Substitution(substituted : String, substituent : Term) {
  def apply(exp : Term) : Term = exp match {
    case Var(name) => if(name.equals(substituted)) substituent else exp
    case Fun(arg, body) => {
      if (!arg.equals(substituted)){
        if(substituent.getFreeVars contains arg)
          apply(new AlphaRenaming(substituent.getFreeVars)(exp))
        else
          Fun(arg, apply(body))
      } else
        exp
    }
    case FApp(t1, t2) => FApp(apply(t1), apply(t2))
    case Let(v,t1,t2) => {
      if (!v.equals(substituted)){
        if(substituent.getFreeVars contains v)
          apply(new AlphaRenaming(substituent.getFreeVars)(exp))
        else
          Let(v,t1,apply(t2))
      } else
        exp
    }
  }
}
