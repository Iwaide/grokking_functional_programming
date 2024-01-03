def extractName(rawShow: String): Option[String] = {
  val bracketOpen = rawShow.indexOf('(')
  if (bracketOpen != -1)
    Some(rawShow.substring(0, bracketOpen).trim)
  else None
}

extractName("Mad Men (-2015)")
extractName("Braking Bad (2008-2013)")
extractName("PiyoPiyo")

def extractYearEnd(rawShow: String): Option[Int] = {
  val dash = rawShow.indexOf('-')
  val bracketClose = rawShow.indexOf(')')
  for {
    yearStr <- if (dash != -1 && bracketClose > dash + 1)
                  Some(rawShow.substring(dash + 1, bracketClose))
                else None
    year <- yearStr.toIntOption
  } yield year
}

extractYearEnd("Mad Men (-2015)")
extractYearEnd("Braking Bad (2008-2013)")
extractYearEnd("PiyoPiyo")

def extractYearStart(rawShow: String): Option[Int] = {
  val dash = rawShow.indexOf('-')
  val bracketOpen = rawShow.indexOf('(')
  for {
    yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
                  Some(rawShow.substring(bracketOpen + 1, dash))
                else None
    year <- yearStr.toIntOption
  } yield year
}

extractYearStart("Mad Men (-2015)")
extractYearStart("Braking Bad (2008-2013)")
extractYearStart("PiyoPiyo")

def extractSingleYear(rawShow: String): Option[Int] = {
  val dash = rawShow.indexOf('-')
  val bracketOpen = rawShow.indexOf('(')
  val bracketClose = rawShow.indexOf(')')

  for {
    yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
                  Some(rawShow.substring(bracketOpen + 1, bracketClose))
                else None
    year <- yearStr.toIntOption
  } yield year
}

extractSingleYear("Mad Men (2015)")
extractSingleYear("Braking Bad (2008-2013)")
extractSingleYear("PiyoPiyo")

List("A (1992-)", "B (2002)", "C (-2012)" , "(2022)", "E (-)").foreach (rawShow => {
  // print(extractSingleYear(rawShow).orElse(extractYearEnd(rawShow)))
  // print(extractYearStart(rawShow).orElse(extractYearEnd(rawShow)).orElse(extractSingleYear(rawShow)))
  // print(for {
  //   validName <- extractName(rawShow)
  //   if validName.length != 0
  //   year <- extractSingleYear(rawShow)
  // } yield year)
  print(extractName(rawShow).flatMap(name => extractYearStart(rawShow).orElse(extractYearEnd(rawShow)).orElse(extractSingleYear(rawShow))))
})