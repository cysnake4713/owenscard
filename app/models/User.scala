package models

import play.api.libs.iteratee.PushEnumerator
import play.api.libs.json.JsValue

/**
 * This code is writed by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/4/13
 * Time: 10:41 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */

class User(val name: String, var status: Boolean, val channel: PushEnumerator[JsValue]) {
  var pokers = List.empty[(String, Int)]
  var showPokers = List.empty[(String, Int)]
  override def toString = {
    "name: " + name + " status:" + status + " channel:" + channel
  }
}
