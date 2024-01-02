case class Book(title: String, authors: List[String])
def recomendationBooks(friend: String): List[Book] = {
  val scala = List(
    Book("FP in Scala", List("Bjarnason")),
    Book("Get Programming with Scala", List("Sfregola")),
  )

  val fiction = List(
    Book("Harry potter", List("Rowling")),
    Book("The Lord of the Ling", List("Tolkin")),
  )
  if(friend == "Alice") scala
  else if (friend == "Bob") fiction
  else List.empty
}
val friends = List("Alice", "Bob", "Charlie")
val recommendations = friends.flatMap(f => recomendationBooks(f))
val authors = recommendations.flatMap(_.authors)
