package ru.spbau.jvm.scala.task01.calculator.parser

import org.scalatest.{BeforeAndAfter, FunSuite}
import ru.spbau.jvm.scala.task01.calculator.{AST, Calculator, CalculatorFunction, CalculatorOperator}

class ParserTest extends FunSuite with BeforeAndAfter {

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

    calculator.registerFunction(new CalculatorFunction("answer") {
      override def eval(arg: Double): Double = arg
    })
  }

  test("parse 1 + (-1)") {
    val string = "1 + (-1)"
    val got = new Parser(calculator).parse(string)
    val expected = AST.BinOperator("+"
      , Some(AST.Number(1))
      , Some(AST.BinOperator("-"
        , Some(AST.Number(0))
        , Some(AST.Number(1)))))

    assert(expected === got)
  }

  test("parse 1 + 1 + 1 + 1 (check associativity)") {
    val string = "1 + 1 + 1 + 1"
    val got = new Parser(calculator).parse(string)
    val expected =
      AST.BinOperator("+"
        , Some(AST.BinOperator("+"
          , Some(AST.BinOperator("+"
            , Some(AST.Number(1))
            , Some(AST.Number(1))))
          , Some(AST.Number(1))))
        , Some(AST.Number(1)))

    assert(expected === got)
  }

  test("parse 1 + 1 + 1 * 1 * 1 + 1") {
    val string = "1 + 1 + 1 * 1 * 1 + 1"
    val got = new Parser(calculator).parse(string)
    val expected =
      AST.BinOperator("+"
        , Some(AST.BinOperator("+"
          , Some(AST.BinOperator("+"
            , Some(AST.Number(1))
            , Some(AST.Number(1))))
          , Some(AST.BinOperator("*"
            , Some(AST.BinOperator("*", Some(AST.Number(1)), Some(AST.Number(1))))
            , Some(AST.Number(1))))))
        , Some(AST.Number(1)))

    assert(expected === got)
  }

  test("parse (1 + 1) + (1 + 1)") {
    val string = "(1 + 1) + ((1 + 1))"
    val got = new Parser(calculator).parse(string)
    val expected = AST.BinOperator("+",
      Some(AST.BinOperator("+", Some(AST.Number(1)), Some(AST.Number(1)))),
      Some(AST.BinOperator("+", Some(AST.Number(1)), Some(AST.Number(1)))))

    assert(expected === got)
  }

  test("parse 1 + answer(42)") {
    val string = "1 + (answer(42))"
    val got = new Parser(calculator).parse(string)
    val expected = AST.BinOperator("+"
      , Some(AST.Number(1))
      , Some(AST.Function("answer", AST.Number(42))))

    assert(expected === got)
  }

  test("fail caused by wrong balance") {
    val string = "(1 + 1) + (1 + 1))"
    val parser = new Parser(calculator)
    intercept[parser.ParserException] {
      parser.parse(string)
    }
  }

  test("fail caused by wrong order") {
    val string = "(1 + 1) + + (1 + 1))"
    val parser = new Parser(calculator)
    intercept[parser.ParserException] {
      parser.parse(string)
    }
  }

  test("fail caused by incomplete expression") {
    val string = "1 + 1 + "
    val parser = new Parser(calculator)
    intercept[parser.ParserException] {
      parser.parse(string)
    }
  }

  test("fail caused by absence of arguments") {
    val string = "answer()"
    val parser = new Parser(calculator)
    intercept[parser.ParserException] {
      parser.parse(string)
    }
  }

  test("fail caused by incomplete function expression") {
    val string = "answer("
    val parser = new Parser(calculator)
    intercept[parser.ParserException] {
      parser.parse(string)
    }
  }
}
