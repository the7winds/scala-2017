package ru.spbau.jvm.scala.task01

import ru.spbau.jvm.scala.task01.calculator.{Calculator, CalculatorOperator}

object Main {
  def main(args: Array[String]): Unit = {
    val input = scala.io.StdIn.readLine()
    val calculator = new Calculator()

      calculator.registerOperator(new CalculatorOperator("+", 1) {
        override def eval(lhs: Double, rhs: Double): Double = lhs + rhs
      })

      calculator.registerOperator(new CalculatorOperator("-", 1) {
        override def eval(lhs: Double, rhs: Double): Double = lhs - rhs
      })

      calculator.registerOperator(new CalculatorOperator("*", 0) {
        override def eval(lhs: Double, rhs: Double): Double = lhs * rhs
      })

      calculator.registerOperator(new CalculatorOperator("/", 0) {
        override def eval(lhs: Double, rhs: Double): Double = lhs / rhs
      })

      println(calculator.submitStringExpression(input) match {
        case Some(result) => result.toString
        case None => "something went wrong"
      })
  }
}
