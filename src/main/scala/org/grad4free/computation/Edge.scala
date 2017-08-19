package org.grad4free
package computation

/**
  * An edge connecting nodes/vertices in a computational graph.
  * It represents an input or output expressions are connected with
  * in an abstract syntax tree representing an arbitrary computation.
  */
sealed trait Edge

/**
  * An edge considered to be variable i.e. w/o a fixed value.
  * @param name Name of the edge.
  * @param value A tensor associated w/ the edge.
  */
case class Variable(name: String, value: Tensor) extends Edge

/**
  * An edge considered to have fixed value.
  * @param name Name of the edge.
  * @param value A tensor associated w/ the edge.
  */
case class Constant(name: String, value: Tensor) extends Edge

object Edge {

  implicit class Extensions(val e: Edge) extends AnyVal {
    def name: String = e match {
      case Variable(n, _) => n
      case Constant(n, _) => n
    }

    def value: Tensor = e match {
      case Variable(_, v) => v
      case Constant(_, v) => v
    }
  }

}
