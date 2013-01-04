package models

import util.Random

/**
 * This code is writed by matt.cai and if you want use it, feel free!
 * User: matt.cai
 * Date: 1/4/13
 * Time: 10:42 AM
 * if you have problem here, please contact me: cysnake4713@gmail.com
 */
class Poker {
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
    tempList._1
  }
}
