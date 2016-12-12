package lambda

/**
  * Singleton LambdaTerm Object
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

object LambdaTerm extends LambdaParser{
  def apply(x:String) : Option[Term] =
    (parse(x)) match {
      case Success(expr, _) => Some(expr)
      case NoSuccess(_) => None
    }
}
