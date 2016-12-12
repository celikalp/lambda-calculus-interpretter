package lambda

/**
  * Given a lambda term, LambdaPrettyPrinter defines its mathematical representation
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

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
