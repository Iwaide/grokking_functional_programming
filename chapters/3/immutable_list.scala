object ImmutableList {
  def firstTwo(list: List[String]): List[String] = {
    list.take(2)
  }

  def lastTwo(list: List[String]): List[String] = {
    list.takeRight(2)
  }

  def movedFirstTwoToTheEnd(list: List[String]): List[String] = {
    list.drop(2) ++ list.take(2)
  }

  def insertBeforeLast(list: List[String], elem: String): List[String] = {
    val init = list.init
    val last = list.last
    init :+ elem :+ last
  }
}