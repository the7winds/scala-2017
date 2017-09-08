package ru.spbau.jvm.scala.task01.calculator.parser

import org.scalatest.FunSuite


class TokenizerTest extends FunSuite {

  test("tokenize words") {
    val tokenizer = new Tokenizer("a b c")
    val expected = java.util.Arrays.asList(
      Tokens.Word("a"),
      Tokens.Word("b"),
      Tokens.Word("c"))
    val got = tokenizer.tokenize()
    assert(expected == got)
  }

  test("tokenize words with parenthesis and numbers") {
    val tokenizer = new Tokenizer("a(b1(42))")
    val expected = java.util.Arrays.asList(
      Tokens.Word("a"),
      Tokens.LeftParenthesis(),
      Tokens.Word("b1"),
      Tokens.LeftParenthesis(),
      Tokens.Number(42),
      Tokens.RightParenthesis(),
      Tokens.RightParenthesis())
    val got = tokenizer.tokenize()
    assert(expected == got)
  }

  test("tokenize expression with different spaces") {
    val tokenizer = new Tokenizer("12+ a   -42")
    val expected = java.util.Arrays.asList(
      Tokens.Number(12),
      Tokens.Operator("+"),
      Tokens.Word("a"),
      Tokens.Operator("-"),
      Tokens.Number(42))
    val got = tokenizer.tokenize()
    assert(expected == got)
  }

  test("fail caused by mixing numbers and letters") {
    val tokenizer = new Tokenizer("4a")
    intercept[TokenizerException] {
      tokenizer.tokenize()
    }
  }
}
