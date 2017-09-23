package ru.spbau.jvm.scala.task03

import java.net.URL
import java.nio.file.Files

import info.mukel.telegrambot4s.models.Message

import scala.sys.process._
import scala.util.Random

class Game(val bot: HangmanBot) {

  private val word = Game.getRandomWord()

  private val representation = {
    val arr = new Array[Char](word.length)
    var i = 0
    for (i <- 0 until arr.length())  {
      arr.update(i, '⬜')
    }

    arr
  }

  final val MAX_LIVES = 5
  var closed = word.length
  var lives = MAX_LIVES

  private def getState(): String = {
    s"""
       |${representation.mkString}
       |❤️: $lives
      """.stripMargin
  }

  def onNew()(implicit message: Message): Unit = {
    bot.reply(getState())
  }

  def finished(): Boolean = closed == 0 || lives == 0

  trait GuessStatus
  case class GuessSuccess() extends GuessStatus
  case class GuessFailed() extends GuessStatus
  case class GuessAlready() extends GuessStatus

  def onTry(letter: Char)(implicit message: Message): Unit = {
    val status = guess(letter)
    bot.reply(status match {
      case GuessSuccess() => "correct"
      case GuessFailed() => "wrong"
      case GuessAlready() => "you already tried this letter"
    })

    if (finished()) {
      status match {
        case GuessSuccess() => bot.reply("Win")
        case GuessFailed() => bot.reply(s"Failed. The word was $word")
      }
    } else {
      bot.reply(getState())
    }
  }

  def guess(letter: Char): GuessStatus = {
    if (!word.contains(letter)) {
      lives -= 1
      return GuessFailed()
    }

    if (representation.contains(letter)) {
      return GuessAlready()
    }

    val i = 0
    for (i <- representation.indices) {
      if (word.charAt(i) == letter) {
        representation.update(i, letter)
        closed -= 1
      }
    }

    return GuessSuccess()
  }
}

object Game {
  private val dictionaryURL = new URL("http://www.desiquintans.com/downloads/nounlist/nounlist.txt")
  private val dictionaryPath = Files.createTempFile("dictionary", "tmp")
  private val _ = { dictionaryURL #> dictionaryPath.toFile !! }
  private val wordsNumber = Files.lines(dictionaryPath).count().asInstanceOf[Int]

  def getRandomWord(): String = {
    val n = new Random().nextInt(wordsNumber)
    Files.lines(dictionaryPath)
      .skip(n)
      .findFirst()
      .get()
      .toUpperCase
  }
}