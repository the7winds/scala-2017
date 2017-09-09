package ru.spbau.jvm.scala.task01.calculator

import org.scalatest.{BeforeAndAfter, FunSuite}

class CalculatorTest extends FunSuite with BeforeAndAfter {
  var calculator = new Calculator()

  before {
    calculator.registerOperator(new CalculatorOperator("+", 1) {
      override def eval(lhs: Double, rhs: Double): Double = lhs + rhs
    })

    calculator.registerOperator(new CalculatorOperator("-", 1) {
      override def eval(lhs: Double, rhs: Double): Double = lhs - rhs
    })

    calculator.registerOperator(new CalculatorOperator("*", 0) {
      override def eval(lhs: Double, rhs: Double): Double = lhs * rhs
    })

    calculator.registerFunction(new CalculatorFunction("sqrt") {
      override def eval(arg: Double): Double = Math.sqrt(arg)
    })
  }

  test("parse 1 + (-1)") {
    val string = "1 + (-1)"
    val Some(got) = calculator.submitStringExpression(string)
    val expected = 0
    assert(expected === got)
  }

  test("parse 1 + (2 - sqrt(4)) - 1") {
    val string = "1 + (2 - sqrt(4)) - 1"
    val Some(got) = calculator.submitStringExpression(string)
    val expected = 0
    assert(expected === got)
  }
}
