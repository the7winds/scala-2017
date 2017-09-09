package ru.spbau.jvm.scala.task01.calculator

abstract class CalculatorOperator(val name: String, val priority: Int) {
  def eval(lhs: Double, rhs: Double): Double
}
