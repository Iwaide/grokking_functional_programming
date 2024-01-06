object model {
  case class Artist(name: String, genre: MusicGenre, origin: Location, yearsActive: YearsActive)

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

  enum YearsActive {
    case StillActive(since: Int)
    case ActiveBetween(start: Int, end: Int)
  }

  def activeLength(artist: Artist, currentYear: Int): Int = {
    artist.yearsActive.match {
      case YearsActive.StillActive(since) => currentYear - since
      case YearsActive.ActiveBetween(start, end) => end - start
    }
  }
}

import model._
activeLength(Artist("Metallica", MusicGenre.HeavyMetal, Location("U.S"), YearsActive.StillActive(1981)), 2022)
activeLength(Artist("Led Zeppelin", MusicGenre.HardRock, Location("England"), YearsActive.ActiveBetween(1968, 1980)), 2022)

