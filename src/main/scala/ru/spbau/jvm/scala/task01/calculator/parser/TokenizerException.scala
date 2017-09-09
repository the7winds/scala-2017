package ru.spbau.jvm.scala.task01.calculator.parser

class TokenizerException(text: String) extends Exception(text) {
  override def getMessage: String = text
}
