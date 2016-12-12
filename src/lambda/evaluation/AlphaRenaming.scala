package lambda.evaluation

import lambda._

/**
  * This class defines alpha renaming
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

class AlphaRenaming(varsNamesToChange : Set[String]) {
  def apply(exp : Term) : Term = exp match {
    case Var(_) => exp
    case Fun(arg, body) => {
      if (varsNamesToChange contains arg) {
        val newArg = getUniqueVarName(arg, varsNamesToChange ++ exp.getBoundVars)
        Fun(newArg, new Substitution(arg, Var(newArg))(body) )
      } else
        Fun(arg, apply(body) )
    }
    case FApp(t1, t2) => FApp(apply(t1), apply(t2))
    case Let(v,t1,t2) => {
      if (varsNamesToChange contains v) {
        val newV = getUniqueVarName(v, varsNamesToChange ++ exp.getBoundVars)
        Let(newV, t1, new Substitution(v, Var(newV))(t2))
      } else
        Let(v,t1,apply(t2))
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
}
