object TipCalculator {
  def getTipPercentage(names: List[String]): Int = {
    if(names.length > 5) {
      20
    } else if(names.length > 0) {
      10
    } else {
      0
    }
  }
}
