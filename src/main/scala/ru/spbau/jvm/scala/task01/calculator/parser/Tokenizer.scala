package ru.spbau.jvm.scala.task01.calculator.parser

import java.util

import ru.spbau.jvm.scala.task01.calculator.parser.Tokens.{_}

class Tokenizer(string: String) {

  val OPERATOR_STRING = "+-/*$#?"

  class TokenizerException(text: String) extends Exception(text)

  private val list = new util.LinkedList[Token]()

  private var word_start = 0
  private var word_end = 0
  private var num_start = 0
  private var num_end = 0
  private var op_start = 0
  private var op_end = 0

  private def flush_word(start: Int): Unit = {
    if (word_start != word_end) {
      list.add(new Word(string.substring(word_start, word_end)))
    }
    word_start = start
    word_end = start
  }

  private def flush_num(start: Int): Unit = {
    if (num_start != num_end) {
      list.add(new Number(string.substring(num_start, num_end).toDouble))
    }
    num_start = start
    num_end = start
  }

  private def flush_op(start: Int): Unit = {
    if (op_start != op_end) {
      list.add(new Operator(string.substring(op_start, op_end)))
    }
    op_end = start
    op_start = start
  }

  private def flush_all(start: Int): Unit = {
    flush_word(start)
    flush_num(start)
    flush_op(start)
  }

  def tokenize(): util.List[Token] = {
    var i = 0
    while (i < string.length()) {
      string.charAt(i) match {
        case ' ' =>
          flush_all(i + 1)
        case '(' =>
          flush_all(i + 1)
          list.add(new LeftParenthesis)
        case ')' =>
          flush_all(i + 1)
          list.add(new RightParenthesis)
        case c if c.isLetter =>
          flush_op(i + 1)
          if (num_start != num_end) {
            throw new TokenizerException("mixing numbers with letters")
          } else {
            word_end += 1
          }
          flush_num(i + 1)
        case c if c.isDigit =>
          flush_op(i + 1)
          if (word_start == word_end) {
            flush_word(i + 1)
            num_end += 1
          } else {
            flush_num(i + 1)
            word_end += 1
          }
        case c if OPERATOR_STRING.contains(c) => {
          flush_num(i + 1)
          flush_word(i + 1)
          op_end += 1
        }
        case _ => throw new TokenizerException("Unrecognized symbol");
      }

      i += 1
    }

    flush_all(i + 1)

    return list
  }
}
