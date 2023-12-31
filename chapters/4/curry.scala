def larger_than(i: Int): List[Int] => List[Int] = {
  nums => nums.filter(num => num >= i)
}
val larger_than_four = larger_than(4)
larger_than_four(List(5, 1, 2, 4, 0))

def divisible_by(i: Int)(nums: List[Int]): List[Int] = {
  nums.filter(num => num % i == 0)
}

divisible_by(5)(List(5, 1, 2, 4, 15))

def shorter_than(i: Int)(words: List[String]): List[String] = {
  words.filter(word => word.length < i)
}

shorter_than(4)(List("scala", "ada"))

def containsS(i: Int)(words: List[String]): List[String] = {
  words.filter(word => word.count { _ == 's'} >= i)
}

containsS(3)(List("rust", "ada"))
