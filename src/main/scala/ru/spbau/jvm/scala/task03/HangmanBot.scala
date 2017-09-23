package ru.spbau.jvm.scala.task03

import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.Message

import scala.collection.mutable

class HangmanBot(val token: String) extends TelegramBot with Polling with Commands {

  val games = new mutable.HashMap[Long, HangmanGame]()

  onMessage {
    implicit msg: Message => {
      games.get(msg.chat.id) match {
        case Some(game) =>
          msg.text.foreach {
            text => {
              val TTT = raw"^\w$$".r.findFirstIn(text)
              TTT match {
                case Some(_) => game.onTry(text.charAt(0).toUpper)
                case _ => ()
              }
            }
          }
        case _ => ()
      }
    }
  }

  private val help =
    """/new to start new game
      |/hint to show hint for the word
      |/help to show this information
    """.stripMargin

  onCommand('start, 'help) {
    implicit msg => reply(help)
  }

  onCommand('new) {
    implicit msg => {
      try {
        val game = new HangmanGame(this)
        games.put(msg.chat.id, game)
        game.onNew()
      } catch {
        case e: Exception => println(e.getMessage)
      }
    }
  }
}
