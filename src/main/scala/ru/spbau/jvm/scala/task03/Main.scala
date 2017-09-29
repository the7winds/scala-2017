package ru.spbau.jvm.scala.task03

object Main extends App {

  private val token = "275348276:AAGJCOuDI_Br-u2V0JAov4gsXWemt-3WqPw"
  private val bot = new HangmanBot(token)

  bot.run()
}
