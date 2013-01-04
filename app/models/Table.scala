package models


object Table {
  var members: Map[String, User] = Map.empty[String, User]
  var memberOrder: List[String] = List(null, null, null, null, null)
  val pokers = new Poker

  def dialPokerToUser(user:User){
    val pokerNum = 5 - user.pokers.size
    dialPokerToUser(user.name, pokerNum)
  }

  private def dialPokerToUser(user: User, pokerNum: Int) {
    dialPokerToUser(user.name, pokerNum)
  }

  private def dialPokerToUser(userName: String, pokerNum: Int) {
    members(userName).pokers = (members(userName).pokers ++ pokers.dialPoker(pokerNum))
      .sortWith((e1, e2) => e1._2 < e2._2)
  }

  def shufflePoker() {
    pokers.shufflePoker()
  }

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

  def clean() {
    members.foreach(ele => ele._2.pokers = List.empty[(String, Int)])
  }
}



