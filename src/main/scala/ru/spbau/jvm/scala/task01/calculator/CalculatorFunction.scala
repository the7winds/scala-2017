package ru.spbau.jvm.scala.task01.calculator

abstract class CalculatorFunction(val name: String) {
  def eval(arg: Double): Double
}
