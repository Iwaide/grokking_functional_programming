case class TvShow(Name: String, startYear: Int, endYear: Int)

def extractYearStart(rawShow: String): Either[String, Int] = {
  val bracketOpen = rawShow.indexOf('(')
  val dash = rawShow.indexOf('-')
  for {
    yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
                  Right(rawShow.substring(bracketOpen + 1, dash))
               else Left(s"Can't extract  start year from $rawShow")
    year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
  } yield year
}

def extractYearEnd(rawShow: String): Either[String, Int] = {
  val bracketClose = rawShow.indexOf(')')
  val dash = rawShow.indexOf('-')
  for {
    yearStr <- if (dash != -1 && bracketClose > dash + 1)
                  Right(rawShow.substring(dash + 1, bracketClose))
               else Left(s"Can't extract year end from $rawShow")
    year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
  } yield year
}

extractYearStart("The Wire (2002-2008)")
extractYearStart("The Wire (-2008)")

def extractName(rawShow: String): Either[String, String] = {
  val bracketOpen = rawShow.indexOf('(')
  for {
    name <- if (bracketOpen != -1)
                  Right(rawShow.substring(0, bracketOpen).trim)
               else Left(s"Can't extract name from $rawShow")
  } yield name
}
extractName("The Wire (2002-2008)")
extractName("The Wire")

def extractSingleYear(rawShow: String): Either[String, Int] = {
  val bracketOpen = rawShow.indexOf('(')
  val bracketClose = rawShow.indexOf(')')
  val dash = rawShow.indexOf('-')
  for {
    yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
                  Right(rawShow.substring(bracketOpen + 1, bracketClose))
               else Left(s"Can't extract single year from $rawShow")
    year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
  } yield year
}

extractSingleYear("The Wire (2002-2008)")
extractSingleYear("The Wire (2002)")

def parseShow(rawShow: String): Either[String, TvShow] = {
  for {
    name <- extractName(rawShow)
    yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
    yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
  } yield TvShow(name, yearStart, yearEnd)
}

parseShow("The Wire (2002-2008)")
parseShow("The Wire (2002)")

def addOrResign(
  parsedShows: Either[String, List[TvShow]],
  newParsedShow: Either[String, TvShow]
): Either[String, List[TvShow]] = {
    for {
      list <- parsedShows
      elem <- newParsedShow
    } yield list.appended(elem)
}


def parseShows(rawShows: List[String]): Either[String, List[TvShow]] = {
  val initialResult: Either[String, List[TvShow]] = Right(List.empty[TvShow])
  rawShows.map(parseShow).foldLeft(initialResult)(addOrResign)
}
parseShows(List("The Wire (2002-2008)", "The Wire (2002)", "Chernobyl"))
