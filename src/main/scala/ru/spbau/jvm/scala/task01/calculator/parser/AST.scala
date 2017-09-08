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

  case class BinOperator(op: String, var lhs: Node, var rhs: Node) extends Node {
    override def eval(calculator: Calculator): Double = {
      calculator.eval_op(op, lhs.eval(calculator), rhs.eval(calculator))
    }
  }


  case class Function(fun: String, arg: Node) extends Node {
    override def eval(calculator: Calculator): Double = {
      calculator.eval_fun(fun, arg.eval(calculator))
    }
  }
}