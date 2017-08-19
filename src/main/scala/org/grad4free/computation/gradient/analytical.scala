package org.grad4free
package computation
package gradient

import cats.data.State
import cats.~>
import cats.implicits._
import syntax._
import Tensor._

object analytical {

  type GradientFunction = (ComputationData, Operation) => ComputationData

  case class ComputationData(values: Map[Operation, Tensor],
                             grads: Map[Operation, Map[Variable, Tensor]],
                             connections: Map[Edge, Operation])

  sealed trait ComputationResult

  object ComputationResult {

    case class Success(output: Variable, gradients: Map[Variable, Tensor]) extends ComputationResult

    case class Failure(error: String) extends ComputationResult

  }

  implicit class ComputationDataExtensions(val d: ComputationData) extends AnyVal {

    def connect(op: Operation, node: Edge): ComputationData =
      d.copy(connections = d.connections.updated(node, op))

    def setValue(op: Operation, value: Tensor): ComputationData =
      d.copy(values = d.values.updated(op, value))

    def setGrad(op: Operation, v: Variable, value: Tensor): ComputationData = {
      val grads = d.grads.getOrElse(op, Map.empty)
      d.copy(grads = d.grads.updated(op, grads.updated(v, value)))
    }

    def mergeGrads(d2: ComputationData, op: Operation): ComputationData = {
      val dg  = d.grads.getOrElse(op, Map.empty)
      val d2g = d2.grads.getOrElse(op, Map.empty)
      d.copy(grads = d.grads.updated(op, dg |+| d2g))
    }

    def mergeIncomingGrads(op: Operation, gradF: GradientFunction*): ComputationData =
      gradF.map(_(d, op)).reduce(_.mergeGrads(_, op))

    def chainGrad(fromOp: Operation, toOp: Operation, f: Tensor => Tensor): ComputationData = {
      val grads = d.grads.getOrElse(fromOp, Map.empty).mapValues(f)
      d.copy(grads = d.grads.updated(toOp, grads))
    }

  }

  type ComputationState[A] = State[ComputationData, A]

  private val interpreter: Op ~> ComputationState = new (Op ~> ComputationState) {

    def sumGrad(x: Edge)(d: ComputationData, op: Operation): ComputationData =
      (x, d.connections.get(x)) match {
        case (v: Variable, None) => d.setGrad(op, v, 1.0)
        case (v, Some(iop)) => d.chainGrad(iop, op, _ * 1.0)
        case _ => d
      }

    def substractGrad(x: Edge, subtrahend: Boolean)(d: ComputationData, op: Operation): ComputationData = {
      val factor = if (subtrahend) -1.0 else 1.0

      (x, d.connections.get(x)) match {
        case (vx: Variable, None) =>
          d.setGrad(op, vx, factor)
        case (vx, Some(fromOp)) =>
          d.chainGrad(fromOp, op, _ * factor)
        case _ => d
      }
    }

    def multiplyGrad(x: Edge, y: Edge)(d: ComputationData, op: Operation): ComputationData =
      (x, d.connections.get(x), y) match {
        case (vx: Variable, None, vy) =>
          d.setGrad(op, vx, vy.value)
        case (vx, Some(fromOp), vy) =>
          d.chainGrad(fromOp, op, _ * vy.value)
        case _ => d
      }

    def apply[A](op: Op[A]): ComputationState[A] = op match {

      case op@SumOp(x, y) =>
        State {
          s =>
            val output = Variable(s"(${x.name} + ${y.name})", x.value + y.value)
            s.setValue(op, x.value + y.value)
              .connect(op, output)
              .mergeIncomingGrads(op, sumGrad(x), sumGrad(y)) -> output
        }
      case op@SubtractOp(x, y) =>
        State {
          s =>
            val output = Variable(s"(${x.name} - ${y.name})", x.value - y.value)
            s.setValue(op, x.value - y.value)
              .connect(op, output)
              .mergeIncomingGrads(op,
                substractGrad(x, subtrahend = false),
                substractGrad(y, subtrahend = true)) -> output
        }
      case op@MultiplyOp(x, y) =>
        State {
          s =>
            val output = Variable(s"(${x.name} * ${y.name})", x.value * y.value)
            s.setValue(op, x.value * y.value)
              .connect(op, output)
              .mergeIncomingGrads(op, multiplyGrad(x, y), multiplyGrad(y, x)) -> output
        }
    }
  }

  def apply(comp: Computation[Edge]): ComputationResult = {
    val (compData, output) = comp.foldMap(interpreter).run(
      ComputationData(values = Map.empty, grads = Map.empty, connections = Map.empty)).value

    (output, compData.connections.get(output)) match {
      case (v: Variable, Some(op)) =>
        ComputationResult.Success(output = v, gradients = compData.grads.getOrElse(op, Map.empty))
      case _ => ComputationResult.Failure("Computation failed")
    }
  }

}
