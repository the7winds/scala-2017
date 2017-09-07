package ru.spbau.jvm.scala.task01

import java.io.DataInputStream

import ru.spbau.jvm.scala.task01.calculator.{Calculator, CalculatorOperator}

object Main {
  def main(args: Array[String]): Unit = {
    val dataInput = new DataInputStream(System.in)
    val inputStr = dataInput.readUTF()
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

    System.out.println(calculator.submitStringExpression(inputStr))
  }
}
