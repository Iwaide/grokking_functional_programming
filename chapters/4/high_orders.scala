def higher_than4(i: Int): Boolean = {
  i >= 4
}
List(5, 1, 2, 4, 0).filter(higher_than4)

def higher_than(threshold: Int): Int => Boolean = {
  i => i >= threshold
}

val higher_than1 = higher_than(1)
List(5, 1, 2, 4, 0).filter(higher_than1)

def multiple_of(radix: Int): Int => Boolean = {
  i => i % radix == 0
}

val multiple_of_five = multiple_of(5)
val multiple_of_two = multiple_of(2)
List(5, 1, 2, 4, 15).filter(multiple_of_five)
List(5, 1, 2, 4, 15).filter(multiple_of_two)

def length_shorter_than(threshold: Int): String => Boolean = {
  word => word.length < threshold
}

val length_shorter_than_four = length_shorter_than(4)
val length_shorter_than_seven = length_shorter_than(7)
List("scala", "ada").filter(length_shorter_than_four)
List("scala", "ada").filter(length_shorter_than_seven)

def moreThanOrEqualCountS(i: Int): String => Boolean = {
  word => word.count { _ == 's' } >= i
}

val moreThanOrEqualOne = moreThanOrEqualCountS(1)
val moreThanOrEqualThree = moreThanOrEqualCountS(3)

List("rust", "ada").filter(moreThanOrEqualOne)
List("rust", "ada").filter(moreThanOrEqualThree)
