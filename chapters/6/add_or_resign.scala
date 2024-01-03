case class TvShow(Name: String, startYear: Int, endYear: Int)

def merge(list: Option[List[TvShow]], elem: TvShow): Option[List[TvShow]] = {
  list.flatMap(l => Some(l.appended(elem))).orElse(None)
}

def addOrResign(
  parsedShows: Option[List[TvShow]],
  newParsedShow: Option[TvShow]
): Option[List[TvShow]] = {
  // newParsedShow.
    // flatMap(newShow => {
    //   parsedShows.flatMap(l => {
    //     Some(l.appended(newShow))
    //   })
    // }).
    // orElse(None)
    for {
      list <- parsedShows
      elem <- newParsedShow
    } yield list.appended(elem)
}

addOrResign(Some(List.empty), Some(TvShow("Chernobyl", 2019, 2019)))

def xParsedShows(rawShows: List[String]): Option[List[TvShow]] = {
  val initialResult: Option[List[TvShow]] = Some(list.empty)
  rawShows.
    map(parseShow).
    foldLeft(initialResult)(addOrResign)
}
