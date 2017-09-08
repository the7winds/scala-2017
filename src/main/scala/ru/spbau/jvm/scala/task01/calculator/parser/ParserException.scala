package ru.spbau.jvm.scala.task01.calculator.parser

class ParserException(description: String) extends Exception {
  override def getMessage: String = description
}
