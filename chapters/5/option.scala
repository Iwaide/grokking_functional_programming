case class Event(name: String, start: Int, end: Int)

def validateLength(start: Int, end: Int, minLength: Int): Option[Int] = {
  val length = end - start
  if (length >= minLength) Some(length) else None
}

def validateName(name: String): Option[String] = {
  if (name.size > 0) Some(name) else None
}

def validateEnd(end: Int): Option[Int] = {
  if (end < 3000) Some(end) else None
}

def validateStart(start: Int, end: Int): Option[Int] = {
  if (start <= end) Some(start) else None
}

def parseLongEvent(name: String, start: Int, end: Int, minLength: Int): Option[Event] = {
  for {
    validName <- validateName(name)
    validEnd <- validateEnd(end)
    validStart <- validateStart(start, validEnd)
    validLength <- validateLength(validStart, validEnd, minLength)
  } yield Event(validName, validStart, validEnd)
}

parseLongEvent("Apollo Program", 1961, 1972, 10)
parseLongEvent("World War II", 1939, 1945, 10)