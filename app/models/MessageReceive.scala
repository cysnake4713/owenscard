package models

import play.api.libs.json.JsValue

/**
 * This code is written by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/4/13
 * Time: 11:19 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
object MessageReceive {
  def parseSwitchCards(jsValue: JsValue): List[(String, Int)] = {
    val switchPokersColor = (jsValue \ "message" \\ "color").map(_.as[String])
    val switchPokersNum = (jsValue \ "message" \\ "number").map(_.as[Int])
    (switchPokersColor zip switchPokersNum).toList
  }

  def parseShowCards(jsValue: JsValue): List[(String, Int)] = {
    val showPokersColor = (jsValue \ "message" \\ "color").map(ele => ele.as[String])
    val showPokersNum = (jsValue \ "message" \\ "number").map(ele => ele.as[Int])
    (showPokersColor zip showPokersNum).toList

  }

//  def parseUnShowCards(showPoker: List[(String, Int)]): List[(String, Int)] = {
//    Table.members(name).pokers.filterNot(ele => showPoker.contains(ele))
//  }
}
