package models

class Pokers private (val color: String, val number: Int) {
}

object Pokers {
  val color = Set("diamond", "club",
    "hearts",
    "spade")
  val number = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  val pokers = color flatMap { x => number map (y => (x, y)) }

  //use Fisher_Yates算法
  def ShufflePoker()={
    var pokerList = pokers.toList
    pokerList
  }
}

object test extends App {
	println(Pokers.pokers.toList)
}