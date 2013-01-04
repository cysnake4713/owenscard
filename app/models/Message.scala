package models

import play.api.libs.json.{JsArray, JsString, JsObject, Json}
import play.api.libs.json.Json._
import play.Logger

/**
 * All the code is write by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/3/13
 * Time: 11:11 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
object Message {

  def finished() {
    notifyAll("finished", "", "")
  }

  def openAll(userName: String, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyAll("opencard", userName, jsonObj.toString())
  }

  def fold(userName: String) {
    notifyAll("foldresult", userName, "")
  }

  def end() {
    notifyAll("toEnd", "", "")
  }

  def show(userName: String, showPoker: List[(String, Int)], unShowPoker: List[(String, Int)]) {
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

    Table.members.foreach {
      case (name, user) if (name == userName) =>
        val result = toJson(Map("showPoker" -> showPokerJs,
          "unShowPoker" -> unShowPokerJs))
        notifyOne("showself", name, result.toString())
      case (name, user) if (name != userName) =>
        val result = toJson(Map("username" -> toJson(userName), "showPoker" -> showPokerJs, "count" -> toJson(showPoker.size)))
        notifyOne("showother", name, result.toString())
    }
  }

  def switch(userName: String, count: Int, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      Map("cards" -> toJson(
        poker.map(ele =>
          toJson(Map(
            "color" -> toJson(ele._1),
            "number" -> toJson(ele._2))))),
        "count" -> toJson(count)))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne("switchresult", userName, jsonObj.toString())
  }

  def sendMember(userName: String) {
    notifyOne("members", userName, "")
  }

  def quit(userName: String) {
    notifyAll("quit", userName, "")
  }

  def join(user: String) {
    notifyAll("members", user, "")
  }

  def dial(userName: String, poker: List[(String, Int)]) {
    val jsonObj = Json.toJson(
      poker.map(ele =>
        toJson(Map(
          "color" -> toJson(ele._1),
          "number" -> toJson(ele._2)))))
    Logger.debug("dial:generate jsonObj=" + jsonObj)
    notifyOne("dial", userName, jsonObj.toString())
  }

  def notifyOne(kind: String, userName: String, text: String) {
    val index = Table.memberOrder.indexWhere(_ == userName)
    val memberForJS = Table.memberOrder.slice(index, Table.memberOrder.size) ::: Table.memberOrder.slice(0, index)
    val msg = JsObject(
      Seq(
        "kind" -> JsString(kind),
        "user" -> JsString(userName),
        "message" -> JsString(text),
        "members" -> JsArray(
          memberForJS.map(JsString))))
    Logger.debug("notifyOne: send message:" + msg)
    Table.members.foreach {
      case (key, user) if key == userName =>
        user.channel.push(msg)
      case _ =>
    }
  }

  def notifyAll(kind: String, user: String, text: String) {
    Table.members.foreach {
      case (userName, ele) => {
        val index = Table.memberOrder.indexWhere(_ == userName)
        val memberForJS = Table.memberOrder.slice(index, Table.memberOrder.size) ::: Table.memberOrder.slice(0, index)
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


