package ru.spbau.jvm.scala.task01.calculator

package object AST {
  trait Node {
    def eval(calculator: Calculator): Double
  }

  case class Number(number: Double) extends Node {
    override def eval(calculator: Calculator): Double = {
      number
    }
  }

  case class BinOperator(op: String, var lhs: Option[Node], var rhs: Option[Node]) extends Node {
    override def eval(calculator: Calculator): Double = {
      calculator.eval_op(op, lhs.get.eval(calculator), rhs.get.eval(calculator))
    }
  }


  case class Function(fun: String, arg: Node) extends Node {
    override def eval(calculator: Calculator): Double = {
      calculator.eval_fun(fun, arg.eval(calculator))
    }
  }
}