object Abbreviate {
  def execute(fullName: String): String = {
    val firstName = fullName.split(" ")(0)
    val initial = firstName(0).toUpper
    val lastName = fullName.split(" ")(1)
    s"$initial. $lastName"
  }
}
