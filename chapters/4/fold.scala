def total(nums: List[Int]): Int = {
  nums.foldLeft(0)((total, num) => total + num)
}
total(List(5, 1, 2, 4, 100))

def total_length(words: List[String]): Int = {
  words.foldLeft(0)((total, word) => total + word.length)
}

total_length(List("scala", "rust", "ada"))

def totalS(words: List[String]): Int = {
  words.foldLeft(0)((total, word) => total + word.count { _ == 's'})
}
totalS(List("scala", "haskell", "rust", "ada"))

def max(nums: List[Int]): Int = {
  nums.foldLeft(Int.MinValue)((acc, num) => if(acc >= num) acc else num)
}

max(List(5, 1, 2, 4, 15))
