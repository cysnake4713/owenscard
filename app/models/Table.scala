package models


class Table {
  var gamingMembers: Map[String, User] = Map.empty[String, User]
  var preparingMembers: Map[String, User] = Map.empty[String, User]
  var memberOrder: List[String] = List(null, null, null, null, null)
  val pokers = new Poker
  var isStarted = false

  def dialPokerToUser(user: User) {
    val pokerNum = 5 - user.pokers.size
    dialPokerToUser(user, pokerNum)
  }

  private def dialPokerToUser(user: User, pokerNum: Int) {
    dialPokerToUser(user.name, pokerNum)
  }

  private def dialPokerToUser(userName: String, pokerNum: Int) {
    gamingMembers(userName).pokers = (gamingMembers(userName).pokers ++ pokers.dialPoker(pokerNum))
      .sortWith((e1, e2) => e1._2 < e2._2)
  }

  def shufflePoker() {
    pokers.shufflePoker()
  }

  def +(user: User) {
    add(user)
  }

  def add(user: User) {
    if (!isFull) {
      preparingMembers += (user.name -> user)
      val index = memberOrder.indexWhere(_ == null)
      memberOrder = memberOrder.slice(0, index) ::: List(user.name) ::: memberOrder.slice(index + 1, memberOrder.size)
    }
  }

  def contains(userName: String): Boolean = {
    gamingMembers.contains(userName) || preparingMembers.contains(userName)
  }

  def size(): Int = {
    gamingMembers.size + preparingMembers.size
  }


  def -(userName: String) {
    gamingMembers -= userName
    preparingMembers -= userName
    val index = memberOrder.indexWhere(_ == userName)
    memberOrder = memberOrder.slice(0, index) ::: List(null) ::: memberOrder.slice(index + 1, memberOrder.size)
  }

  def isFull: Boolean = {
    if (gamingMembers.size + preparingMembers.size >= 5) {
      true
    } else {
      false
    }
  }

  def isReadyPrepare = {
    (true /: preparingMembers) {
      (result, ele) => result && ele._2.status
    }
  }

  def isReadyGaming = {
    (true /: gamingMembers) {
      (result, ele) => result && ele._2.status
    }
  }

  def setAllNotReady() {
    gamingMembers.foreach {
      case (name, user) => user.status = false
    }
  }

  def clean() {
    gamingMembers.foreach(ele => {
      ele._2.pokers = List.empty[(String, Int)]
      ele._2.showPokers = List.empty[(String, Int)]
    })
  }

  def startGame() {
    isStarted = true
    gamingMembers = preparingMembers
    preparingMembers = Map.empty[String, User]
  }

  def endGame() {
    isStarted = false
    preparingMembers = preparingMembers ++ gamingMembers
    gamingMembers = Map.empty[String, User]
  }

}



