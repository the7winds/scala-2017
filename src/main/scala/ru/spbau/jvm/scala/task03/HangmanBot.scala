package ru.spbau.jvm.scala.task03

import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.SendGame
import info.mukel.telegrambot4s.models.{InlineKeyboardButton, InlineKeyboardMarkup, Message}

import scala.collection.mutable

class HangmanBot(val token: String) extends TelegramBot with Polling with Commands {

  val games = new mutable.HashMap[Long, Game]()

  onMessage {
    implicit msg: Message => {
      games.get(msg.chat.id) match {
        case Some(game) =>
          if (game.finished()) {
            games.remove(msg.chat.id)
          } else {
            msg.text.foreach {
              text => {
                val TTT = raw"^\w$$".r.findFirstIn(text)
                TTT match {
                  case Some(_) => game.onTry(text.charAt(0).toUpper)
                  case _ => ()
                }
              }
            }
          }
        case _ => ()
      }
    }
  }

  private val help =
    """Traditional game. Bot make a word, you guess it.
      |/new to start new game
      |/help to show this information
    """.stripMargin

  onCommand('start, 'help) {
    implicit msg => reply(help)
  }

  onCommand('new) {
    implicit msg => {
      val game = new Game(this)
      games.put(msg.chat.id, game)
      game.onNew()
    }
  }
}
