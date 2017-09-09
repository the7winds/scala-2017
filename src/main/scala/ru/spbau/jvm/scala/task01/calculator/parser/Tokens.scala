package ru.spbau.jvm.scala.task01.calculator.parser

package object Tokens {

  trait Token

  case class Word(word: String) extends Token

  case class Number(num: Double) extends Token

  case class Operator(op: String) extends Token

  case class LeftParenthesis() extends Token

  case class RightParenthesis() extends Token

}