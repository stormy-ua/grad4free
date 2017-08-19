package org.grad4free
package computation
package optimization

import scala.annotation.tailrec
import syntax._
import gradient.analytical._

object gradientDescend {

  def minimize(computation: EdgeSet => Computation[Edge], input: EdgeSet, iterations: Int, dx: Tensor = 1E-3): EdgeSet = {

    @tailrec
    def go(in: EdgeSet, iter: Int): EdgeSet = {
      val comp = computation(in)
      val result = gradient.analytical(comp)

      result match {
        case ComputationResult.Success(output, gradients) =>

          println(s"$iter - $output")

          val adjIn = in.map {
            case (name, v: Variable) =>
              val grad = gradients.find { case (k, _) => k.name == name }.get._2
              name -> v.copy(value = v.value - dx * grad)
            case kv => kv
          }

          if (iter > 0) go(adjIn, iter - 1) else in

        case ComputationResult.Failure(error) => ???

      }
    }

    go(input, iterations)
  }

}
