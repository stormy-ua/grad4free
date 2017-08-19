package org.grad4free

import computation._
import syntax._
import gradient.analytical._
import optimization._

object Main {

  def main(args: Array[String]): Unit = {

    val x1 = Variable("x1", 1.0)
    val x2 = Variable("x2", 2.0)
    val x3 = Variable("x3", 3.0)
    val c1 = Constant("c1", 2.0)

    val input = Map(
      "x1" -> x1,
      "x2" -> x2,
      "x3" -> x3,
      "c1" -> c1
    )

    def computation[A](input: Map[String, Edge]) = for {
      sum1 <- subtract(input("x1"), input("x2"))
      sum2 <- sum(sum1, input("x3"))
      mul1 <- multiply(sum2, input("c1"))
      mul2 <- multiply(mul1, mul1)
    } yield mul2

    val comp = computation(input)
    val result = gradient.analytical(comp)

    result match {
      case ComputationResult.Success(output, gradients) =>
        println(s"f = ${output.name}")
        println(s"x1 = ${x1.value}")
        println(s"x2 = ${x2.value}")
        println(s"x3 = ${x3.value}")
        println(s"f(x1, x2, x3) = ${output.value}")
        println(s"analytical df/dx1 = ${gradients.get(x1)}")
        println(s"analytical df/dx2 = ${gradients.get(x2)}")
        println(s"analytical df/dx3 = ${gradients.get(x3)}")
      case ComputationResult.Failure(error) => println(s"computation failed: $error")
    }

    val dfdx1 = gradient.numerical(computation, input, "x1")
    println(s"numerical df/dx1 = $dfdx1")
    val dfdx2 = gradient.numerical(computation, input, "x2")
    println(s"numerical df/dx1 = $dfdx2")
    val dfdx3 = gradient.numerical(computation, input, "x3")
    println(s"numerical df/dx1 = $dfdx3")

    val optimized = gradientDescend.minimize(computation, input, 500)

    println(s"optimized = $optimized")

    println(s"value w/ optimized variables = ${gradient.analytical(computation(optimized))}")

  }

}
