package lambda.evaluation

/**
  * Enumaration of evaluations strategies
  * @author Ahmet Alparslan Celik
  * @version  0.1
  * @since  0.1
  */

trait EvaluationStrategy
case object FULL_BETA extends EvaluationStrategy
case object NORMAL_ORDER extends EvaluationStrategy
case object CALL_BY_VALUE extends EvaluationStrategy
case object CALL_BY_NAME extends EvaluationStrategy
