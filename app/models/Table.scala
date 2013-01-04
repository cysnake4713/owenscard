package models

import scala.util.Random
import play.api.libs.iteratee.PushEnumerator
import play.api.libs.json.JsValue


object Table {
  var members: Map[String, User] = Map.empty[String, User]
  var memberOrder: List[String] = List(null, null, null, null, null)

  def +(user: User) {
    add(user)
  }

  def contains(userName: String): Boolean = {
    members.contains(userName)
  }

  def size(): Int = {
    members.size
  }

  def add(user: User) {
    if (!isFull) {
      members += (user.name -> user)
      val index = memberOrder.indexWhere(_ == null)
      memberOrder = memberOrder.slice(0, index) ::: List(user.name) ::: memberOrder.slice(index + 1, memberOrder.size)
    }
  }


  def -(userName: String) {
    members -= userName
    val index = memberOrder.indexWhere(_ == userName)
    memberOrder = memberOrder.slice(0, index) ::: List(null) ::: memberOrder.slice(index + 1, memberOrder.size)
  }

  def isFull: Boolean = {
    if (members.size >= 5) {
      true
    } else {
      false
    }
  }

  def isReady = {
    (true /: members) {
      (result, ele) => result && ele._2.status
    }
  }

  def setAllNotReady() {
    members.foreach {
      case (name, user) => user.status = false
    }
  }
}

class User(val name: String, var status: Boolean, val channel: PushEnumerator[JsValue]) {
  var pokers = List.empty[(String, Int)]

  override def toString = {
    "name: " + name + " status:" + status + " channel:" + channel
  }
}

object Poker {
  private val color = Set("DIAMOND", "CLUB", "HEARTS", "SPADE")
  private val number = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  private val pokers = color flatMap {
    x => number map (y => (x, y))
  }
  private var currentPokers = List.empty[(String, Int)]

  /**
   * get A new shuffled poker list
   * return List
   */
  private def getShuffledPoker = {
    //use Fisher_Yates算法
    var pokerList = pokers.toList
    //    var pokerList = number.toList
    for (i <- 0 to pokerList.size - 2) {
      val j = i + Random.nextInt(pokerList.size - i)
      val temp = pokerList(i)
      pokerList = pokerList.updated(i, pokerList(j))
      pokerList = pokerList.updated(j, temp)
    }
    pokerList
  }

  def shufflePoker() {
    this.currentPokers = getShuffledPoker
  }

  def dialPoker(number: Int) = {
    if (currentPokers.size < number) shufflePoker()
    val tempList = currentPokers.splitAt(number)
    currentPokers = tempList._2
    tempList._1.sortWith((e1, e2) => e1._2 < e2._2)
  }
}
