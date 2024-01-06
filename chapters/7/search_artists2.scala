object model {
  opaque type Location = String
  object Location {
    def apply(value: String): Location = value
    extension (a: Location) def name: String = a
  }

  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }

  case class ActivePeriod(
    start: Int,
    end: Int
  )

  enum YearsActive {
    case StillActive(since: Int, periods: List[ActivePeriod])
    case ActiveBetweens(periods: List[ActivePeriod])
  }

  case class Artist(
    name: String,
    genre: MusicGenre,
    origin: Location,
    yearsActive: YearsActive
  )

  enum SearchCondition {
    case SearchByGenre(genres: List[MusicGenre])
    case SearchByOrigin(locations: List[Location])
    case SearchByActivePeriod(period: ActivePeriod)
    case SearchByActiveLength(years: Int, until: Int)
  }
}

import model._, model.MusicGenre._, model.YearsActive._, model.SearchCondition._

def wasArtistActive(artist: Artist, activePeriod: ActivePeriod): Boolean = {
  artist.yearsActive match {
    case StillActive(since, periods) => since <= activePeriod.end || periods.exists(period => period.start <= activePeriod.end && period.end >= activePeriod.start)
    case ActiveBetweens(periods) => periods.exists(period => period.start <= activePeriod.end && period.end >= activePeriod.start)
  }
}

def activeLength(artist: Artist, currentYear: Int): Int = {
  val periods =
    artist.yearsActive match {
      case StillActive(since, periods) => periods.appended(ActivePeriod(since, currentYear))
      case ActiveBetweens(periods) => periods
    }
  periods.map(p => p.end - p.start).foldLeft(0)((x, y) => x + y)
}

def searchArtists(
  artists: List[Artist],
  requiredConditions: List[SearchCondition]
): List[Artist] = {
  artists.filter(artist => {
    requiredConditions.forall(condition => {
      condition.match {
        case SearchByGenre(genres) => genres.contains(artist.genre)
        case SearchByOrigin(locations) => locations.contains(artist.origin)
        case SearchByActivePeriod(period) => wasArtistActive(artist, period)
        case SearchByActiveLength(howLong, until) => activeLength(artist, until) >= howLong
      }
    })
  })
}

