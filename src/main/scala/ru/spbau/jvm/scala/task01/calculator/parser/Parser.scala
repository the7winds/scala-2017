package ru.spbau.jvm.scala.task01.calculator.parser

import java.util

import ru.spbau.jvm.scala.task01.calculator.{AST, Calculator}

class Parser(calculator: Calculator) {

  trait ContextType
  case class Function(name: String) extends ContextType
  case class Parenthesis() extends ContextType
  case class Global() extends ContextType

  class ParseContext(val contextType: ContextType) {

    val priorityStacks = new util.HashMap[Int, util.Stack[Tokens.Operator]]
    val treeStack = new util.Stack[AST.Node]

    def getHigherThan(priority: Int): Boolean = {
      var has = false
      priorityStacks.entrySet().forEach(e => has |= e.getKey < priority && !e.getValue.empty)
      return has
    }

    def foldStacks(): Unit = {
      val iter = priorityStacks.entrySet().iterator()

      while (iter.hasNext) {
        val e = iter.next()
        val priority = e.getKey
        val ops_stack = e.getValue
        if (!ops_stack.empty()) {
          val Tokens.Operator(op) = ops_stack.pop()
          val tree = AST.BinOperator(op, None, Some(treeStack.pop()))
          var prev = tree

          while (!ops_stack.empty()) {
            val Tokens.Operator(op) = ops_stack.pop()
            val cur = AST.BinOperator(op, None, Some(treeStack.pop()))
            prev.lhs = Some(cur)
            prev = cur
          }

          prev.lhs = Some(treeStack.pop())

          treeStack.push(tree)
        }
      }
    }
  }

  private def getPriority(op: String) = calculator.getOpPriority(op)

  private def parseImpl(tokens: util.List[Tokens.Token]): AST.Node = {
    val context = new util.Stack[ParseContext]()
    context.push(new ParseContext(this.Global()))

    val iter = tokens.iterator()

    while (iter.hasNext) {
      val token = iter.next()
      token match {
        case Tokens.Number(n) => {
          context.peek().treeStack.push(AST.Number(n))
        }
        case fun @ Tokens.Word(funName) => {
          iter.next() match {
            case Tokens.LeftParenthesis() => {
              context.push(new ParseContext(this.Function(funName)))
            }
            case _ => throw new ParserException("function without arguments")
          }
        }
        case op @ Tokens.Operator(str) => {
          val priority = getPriority(str)
          val ctx = context.peek()
          if (ctx.getHigherThan(priority)) {
            ctx.foldStacks()
          }
          ctx.priorityStacks.computeIfAbsent(priority, _ => new util.Stack[Tokens.Operator]).push(op)
        }
        case Tokens.RightParenthesis() => {
          val ctx = context.pop()
          ctx.foldStacks()
          val tree = ctx.treeStack.pop()
          ctx.contextType match {
            case this.Function(name) => context.peek().treeStack.push(AST.Function(name, tree))
            case this.Parenthesis() => context.peek().treeStack.push(tree)
          }
        }
        case Tokens.LeftParenthesis() => {
          context.push(new ParseContext(this.Parenthesis()))
        }
      }
    }

    context.peek().foldStacks()
    context.pop().treeStack.pop()
  }

  class ParserException(text: String) extends Exception(text)

  private def validateSyntaxAndFixUnarMinus(tokens: util.List[Tokens.Token]): util.List[Tokens.Token] = {
    var balance = 0
    var prev: Tokens.Token = null
    val handledTokens = new util.LinkedList[Tokens.Token]()
    val iter = tokens.iterator()

    while (iter.hasNext) {
      val token = iter.next()

      if (balance < 0) {
        throw new ParserException("impossible balance")
      }

      token match {
        case Tokens.LeftParenthesis()
          if iter.hasNext &&
            (prev == null
              || prev.isInstanceOf[Tokens.Operator]
              || prev.isInstanceOf[Tokens.Word]
              || prev.isInstanceOf[Tokens.LeftParenthesis]) => {
          balance += 1
          prev = token
        }
        case Tokens.RightParenthesis()
          if prev.isInstanceOf[Tokens.Number]
            || prev.isInstanceOf[Tokens.Word]
            || prev.isInstanceOf[Tokens.RightParenthesis] => {
          balance -= 1
          prev = token
        }
        case Tokens.Operator(_)
          if iter.hasNext &&
            (prev.isInstanceOf[Tokens.Number]
              || prev.isInstanceOf[Tokens.Word]
              || prev.isInstanceOf[Tokens.RightParenthesis]) => {
          prev = token
        }
        case Tokens.Operator("-")
          if iter.hasNext &&
            prev.isInstanceOf[Tokens.LeftParenthesis] => {
          handledTokens.add(Tokens.Number(0.toDouble))
          prev = token
        }
        case Tokens.Word(_)
          if iter.hasNext &&
            (prev == null
              || prev.isInstanceOf[Tokens.Operator]
              || prev.isInstanceOf[Tokens.LeftParenthesis]) => {
          prev = token
        }
        case Tokens.Number(_)
          if prev == null
            || prev.isInstanceOf[Tokens.Operator]
            || prev.isInstanceOf[Tokens.LeftParenthesis] => {
          prev = token
        }
        case _ => throw new ParserException("wrong order")
      }

      handledTokens.add(token)
    }

    if (balance != 0) {
      throw new ParserException("impossible balance")
    }

    return handledTokens
  }

  def parse(string: String): AST.Node = {
    var tokens = new Tokenizer(string).tokenize()
    tokens = validateSyntaxAndFixUnarMinus(tokens)
    return parseImpl(tokens)
  }
}
