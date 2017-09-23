package ru.spbau.jvm.scala.task03

import info.mukel.telegrambot4s.models.Message

trait GuessStatus
case class GuessSuccess() extends GuessStatus
case class GuessFailed() extends GuessStatus
case class GuessAlready() extends GuessStatus

class HangmanGame(val bot: HangmanBot) {

  private val context = new HangmanContext("ALPHABET", "letters")

  private def getState(): String = {
    s"""
       |${context.representation.mkString}
       |lives: ${context.lives}
      """.stripMargin
  }

  private def getHint(): String = s"Hint: ${context.hint}"

  def onNew()(implicit message: Message): Unit = {
    bot.reply(getState())
  }

  def onTry(letter: Char)(implicit message: Message): Unit = {
    val status = context.guess(letter)
    bot.reply(status match {
      case GuessSuccess() => "correct"
      case GuessFailed() => "wrong"
      case GuessAlready() => "you already tried this letter"
    })

    bot.reply(getState())

    if (context.gameFinished()) {
      status match {
        case GuessSuccess() => bot.reply("Win")
        case GuessFailed() => bot.reply("Failed")
      }
    }
  }

  def onHint()(implicit message: Message): Unit = {
    bot.reply(getHint())
  }

  class HangmanContext(val word: String, val hint: String) {
    final val MAX_LIVES = 5

    var closed = word.length
    var lives = MAX_LIVES
    val representation = {
      val arr = new Array[Char](word.length)
      var i = 0
      for (i <- 0 until arr.length())  {
        arr.update(i, '*')
      }

      arr
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

    def gameFinished(): Boolean = closed == 0 || lives == 0
  }
}
