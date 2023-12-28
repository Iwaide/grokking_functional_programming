class TipCalculator {
  public static int tipPercentage(String[] names) {
    if (names.length > 5) {
      return 20;
    } else if (names.length > 0) {
      return 10;
    } else { 0;
      return 0;
    }
  }
}