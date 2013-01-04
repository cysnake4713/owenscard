package models

import play.api.libs.json.{JsArray, JsString, JsObject, Json}
import play.api.libs.json.Json._
import play.Logger

/**
 * All the code is written by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/3/13
 * Time: 11:11 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
object MessageSender {

  def sync(userName: String)(implicit table: Table) {
    sendCurrentMemberToUser(userName)
    notifyOne(table, "sync", "", "")
    for ((name, user) <- table.gamingMembers) {
      if (user.showPokers.nonEmpty) {
        val showPokerJs = Json.toJson(
          user.showPokers.map(ele =>
            toJson(Map(
              "color" -> toJson(ele._1),
              "number" -> toJson(ele._2)))))
        val result = toJson(Map("username" -> toJson(userName),
          "showPoker" -> showPokerJs, "count" -> toJson(user.showPokers.size)))
        notifyOne(table, "showother", userName, result.toString())
      }

    }
  }

  def finished()(implicit table: Table) {
    notifyAll(table, "finished", "", "")
  }

  def open(userName: String, poker: List[(String, Int)])(implicit table: Table) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyAll(table, "opencard", userName, jsonObj.toString())
  }

  def fold(userName: String)(implicit table: Table) {
    notifyAll(table, "foldresult", userName, "")
  }

  def end()(implicit table: Table) {
    notifyAll(table, "toEnd", "", "")
  }

  def show(userName: String, showPoker: List[(String, Int)], unShowPoker: List[(String, Int)])(implicit table: Table) {
    val showPokerJs = Json.toJson(
      showPoker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    val unShowPokerJs = Json.toJson(
      unShowPoker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))

    (table.preparingMembers ++ table.gamingMembers).foreach {
      case (name, user) if (name == userName) =>
        val result = toJson(Map("showPoker" -> showPokerJs,
          "unShowPoker" -> unShowPokerJs))
        notifyOne(table, "showself", name, result.toString())
      case (name, user) if (name != userName) =>
        val result = toJson(Map("username" -> toJson(userName), "showPoker" -> showPokerJs, "count" -> toJson(showPoker.size)))
        notifyOne(table, "showother", name, result.toString())
    }
  }

  def switch(userName: String, count: Int, poker: List[(String, Int)])(implicit table: Table) {
    val jsonObj = Json.toJson(
      Map("cards" -> toJson(
        poker.map(ele =>
          toJson(Map(
            "color" -> toJson(ele._1),
            "number" -> toJson(ele._2))))),
        "count" -> toJson(count)))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne(table, "switchresult", userName, jsonObj.toString())
  }

  def sendCurrentMemberToUser(userName: String)(implicit table: Table) {
    notifyOne(table, "members", userName, "")
  }

  def quit(userName: String)(implicit table: Table) {
    notifyAll(table, "quit", userName, "")
  }

  def join(user: String)(implicit table: Table) {
    notifyAll(table, "members", user, "")
  }

  def dial(userName: String, poker: List[(String, Int)])(implicit table: Table) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne(table, "dial", userName, jsonObj.toString())
  }

  def notifyOne(table: Table, kind: String, userName: String, text: String) {
    val index = table.memberOrder.indexWhere(_ == userName)
    val memberForJS = table.memberOrder.slice(index, table.memberOrder.size) ::: table.memberOrder.slice(0, index)
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(userName),
        "message" -> JsString(text),
        "members" -> JsArray(
          memberForJS.map(JsString))))
    Logger.debug("notifyOne: send message:" + msg)
    (table.preparingMembers ++ table.gamingMembers).foreach {
      case (key, user) if key == userName =>
        user.channel.push(msg)
      case _ =>
    }
  }

  def notifyAll(table: Table, kind: String, user: String, text: String) {
    (table.preparingMembers ++ table.gamingMembers).foreach {
      case (userName, ele) => {
        val index = table.memberOrder.indexWhere(_ == userName)
        val memberForJS = table.memberOrder.slice(index, table.memberOrder.size) ::: table.memberOrder.slice(0, index)
        val msg = JsObject(
          Seq(
            "kind" -> JsString(kind),
            "user" -> JsString(user),
            "message" -> JsString(text),
            "members" -> JsArray(
              memberForJS.map(JsString))))
        Logger.debug("notifyAll:send Message: " + msg)
        ele.channel.push(msg)
      }
    }

  }

}


