package ru.spbau.jvm.scala.task01.calculator

class CalculatorException(description: String) extends Exception {
  override def getMessage: String = description
}
