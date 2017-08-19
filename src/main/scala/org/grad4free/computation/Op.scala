package org.grad4free
package computation

/**
  * Representation of a primitive/atomic operation a computation is build from.
  * It also represents a vertex in a computational graph.
  * @tparam A Type of a result returned by the computation.
  */
sealed trait Op[A]

/**
  * Addition. It takes two incoming edges, and produces
  * an outgoing edge with the tensor equal to the sum of tensors
  * in incoming edges.
  */
case class SumOp[A](x: Edge, y: Edge) extends Op[Edge]

/**
  * Subtraction. It takes two incoming edges, and produces
  * an outgoing edge with the tensor equal to the subtraction of tensors
  * in incoming edges.
  */
case class SubtractOp[A](x: Edge, y: Edge) extends Op[Edge]

/**
  * Multiplication. It takes two incoming edges, and produces
  * an outgoing edge with the tensor equal to the multiplication of tensors
  * in incoming edges.
  */
case class MultiplyOp[A](x: Edge, y: Edge) extends Op[Edge]