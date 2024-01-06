case class User(name: String, city: Option[String], favoriteArtists: List[String])

val users = List(
  User("Alice", Some("Melbourne"), List("Bee Gees")),
  User("Bob", Some("Lagos"), List("Bee Gees")),
  User("Eve", Some("Tokyo"), List.empty),
  User("Mallory", None, List("Metalica")),
  User("Trent", Some("Bueons Aires"), List("Led Zeppelin")),
)

def melbourneOrNone(users: List[User]): List[User] = {
  users.filter(user => {
    user.city.forall(_ == "Melbourne")
  })
}

def lagos(users: List[User]): List[User] = {
  users.filter(user => {
    user.city.exists(_ == "Lagos")
  })
}

def beeGees(users: List[User]): List[User] = {
  users.filter(user => {
    user.favoriteArtists.exists(_ == "Bee Gees")
  })
}

def startT(users: List[User]): List[User] = {
  users.filter(user => {
    user.city.exists(_.charAt(0) == 'T')
  })
}

def eightOrNone(users: List[User]): List[User] = {
  users.filter(user => {
    user.favoriteArtists.forall(_.length > 8)
  })
}

def startM(users: List[User]): List[User] = {
  users.filter(user => {
    user.favoriteArtists.exists(_.charAt(0) == 'M')
  })
}

melbourneOrNone(users)
lagos(users)
beeGees(users)
startT(users)
eightOrNone(users)
startM(users)
