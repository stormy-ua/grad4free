package org.grad4free

import cats.Semigroup

/**
  * Naive tensor representation in a form of a single scalar.
  * In a real-world application it has to accommodate mulltiple values
  * in multiple dimensions so a scalar, a vector, a matrix, and any
  * n-dimensional tensor could be represented.
  * @param value A single scalar value.
  */
case class Tensor(value: Double)

object Tensor {

  implicit class Extensions(val tensor: Tensor) {

    def +(other: Tensor): Tensor = Tensor(tensor.value + other.value)

    def -(other: Tensor): Tensor = Tensor(tensor.value - other.value)

    def *(other: Tensor): Tensor = Tensor(tensor.value * other.value)

    def /(other: Tensor): Tensor = Tensor(tensor.value / other.value)

  }

  implicit def fromDouble(v: Double): Tensor = Tensor(v)

  implicit val semi = new Semigroup[Tensor] {
    def combine(x: Tensor, y: Tensor): Tensor = x + y
  }

}