package org.grad4free
package computation
package gradient

import cats.{Id, ~>}
import syntax._
import Tensor._

/**
  * Free monad interpreter calculating gradients using the numerical approach.
  * Numerical in this case means that only gradient for a single input variable
  * could be calculated per one interpreter run.
  * This is the most generic solution,
  * but not the best from the performance point of view since number of interpretation
  * cycles is equal to the number of input variables multiplied by two.
  */
object numerical {

  private val interpreter: Op ~> Id = new (Op ~> Id) {

    def apply[A](op: Op[A]): A = op match {

      case SumOp(x, y)       => Variable(s"(${x.name} + ${y.name})", x.value + y.value)
      case SubtractOp(x, y)  => Variable(s"(${x.name} - ${y.name})", x.value - y.value)
      case MultiplyOp(x, y)  => Variable(s"(${x.name} * ${y.name})", x.value * y.value)
    }
  }

  /**
    * Computes a gradient for a given computation and a given variable name.
    * @param comp Function feeding set of edges to a computation and producing monadic expression
    *             representing the computation.
    * @param input Set of edges to feed into the computation.
    * @param x Name of the variable to compute gradient for.
    * @param dx Value to vairate input variable value by.
    * @return Tensor representing calculated gradient.
    */
  def apply(comp: EdgeSet => Computation[Edge], input: EdgeSet, x: String, dx: Double = 1E-5): Tensor = {
    val f0 = comp(input).foldMap(interpreter).value
    val f1 = comp(input.map {
      case (k, v: Variable) if k == x => k -> v.copy(value = v.value + dx)
      case kv => kv
    }).foldMap(interpreter).value

    (f1 - f0)/dx
  }

}
