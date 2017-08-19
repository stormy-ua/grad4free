package org.grad4free
package computation

import cats.free.Free

object syntax {

  // Free monad representing an arbitrary computation build
  // from a set of primitive operations.
  type Computation[A] = Free[Op, A]

  def sum[A](x: Edge, y: Edge): Computation[Edge] = Free.liftF[Op, Edge](SumOp(x, y))

  def subtract[A](x: Edge, y: Edge): Computation[Edge] = Free.liftF[Op, Edge](SubtractOp(x, y))

  def multiply[A](x: Edge, y: Edge): Computation[Edge] = Free.liftF[Op, Edge](MultiplyOp(x, y))

}
