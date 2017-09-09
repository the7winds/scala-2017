package ru.spbau.jvm.scala.task01.calculator

import java.util

import ru.spbau.jvm.scala.task01.calculator.parser.{Parser, ParserException, TokenizerException}

class Calculator {
  val operations = new util.HashMap[String, CalculatorOperator]()
  val functions = new util.HashMap[String, CalculatorFunction]()

  def registerFunction(fun: CalculatorFunction): Unit = functions.put(fun.name, fun)

  def registerOperator(op: CalculatorOperator): Unit= operations.put(op.name, op)

  def submitStringExpression(expr: String): Option[Double] = {
    try {
      val parser = new Parser(this)
      val tree = parser.parse(expr)
      return Some(tree.eval(this))
    } catch {
      case e: TokenizerException => System.err.println("[Tokenizer] " + e.getMessage)
      case e: ParserException => System.err.println("[Parser] " + e.getMessage)
      case e: CalculatorException => System.err.println("[Calculator] " + e.getMessage)
      case e: Exception => System.err.println(e.getMessage)
    }

    return None
  }

  def getOpPriority(op: String): Int = {
    try {
      operations.get(op).priority
    } catch {
      case e: Exception => throw new CalculatorException("unknown operator")
    }
  }

  def eval_op(op: String, lhs: Double, rhs: Double): Double = {
    try {
      operations.get(op).eval(lhs, rhs)
    } catch {
      case e: Exception => throw new CalculatorException("unknown operator")
    }
  }

  def eval_fun(fun: String, arg: Double): Double = {
    try {
      functions.get(fun).eval(arg)
    } catch {
      case e: Exception => throw new CalculatorException("unknown function")
    }
  }
}