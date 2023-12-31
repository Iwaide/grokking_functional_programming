def rankedWords(wordScore: String => Int,
  words: List[String]): List[String] = {
    words.sortBy(wordScore).reverse
  }

def score(word: String): Int = word.replaceAll("a", "").length

def bonus(word: String): Int = {
  if (word.contains("c")) 5
  else 0
}

def penalty(word: String): Int = {
  if (word.contains("s")) 7
  else 0
}

def wordScores(wordScore: String => Int,
    words: List[String]): List[Int] = {
      words.map(wordScore)
    } 
def wordLength(word: String): Int = word.length

def countS(word: String): Int = word count { _ == 's'}

def inverseInt(i: Int): Int = -i

def doubleInt(i: Int): Int = i * 2

def filterWords(decision: String => Boolean, words: List[String]): List[String] = {
  words.filter(decision)
}

def isOdd(i: Int): Boolean = i % 2 == 1
