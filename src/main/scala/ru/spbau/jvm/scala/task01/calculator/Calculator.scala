package ru.spbau.jvm.scala.task01.calculator

import java.util

import ru.spbau.jvm.scala.task01.calculator.parser.Parser

class Calculator {
  val operations = new util.HashMap[String, CalculatorOperator]()
  val functions = new util.HashMap[String, CalculatorFunction]()

  def registerFunction(fun: CalculatorFunction): Unit = functions.put(fun.name, fun)

  def registerOperator(op: CalculatorOperator): Unit= operations.put(op.name, op)

  def submitStringExpression(expr: String): Double = {
    val parser = new Parser(this)
    val tree = parser.parse(expr)
    tree.eval(this)
  }

  def getOpPriority(op: String): Int = operations.get(op).priority

  def eval_op(op: String, lhs: Double, rhs: Double): Double = operations.get(op).eval(lhs, rhs)

  def eval_fun(fun: String, arg: Double): Double = functions.get(fun).eval(arg)
}