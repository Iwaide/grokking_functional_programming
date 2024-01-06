object model {
  opaque type Location = String
  object Location {
    def apply(value: String): Location = value
    extension (a: Location) def name: String = a
  }

  opaque type YearsActiveStart = Int
  object YearsActiveStart {
    def apply(value: Int): YearsActiveStart = value
    extension (a: YearsActiveStart) def value: Int = a
  }

  opaque type Genre = String
  object Genre {
    def apply(value: String): Genre = value
    extension (a: Genre) def name: String = a
  }

  opaque type YearsActiveEnd = Int
  object YearsActiveEnd {
    def apply(value: Int): YearsActiveEnd = value
    extension (a: YearsActiveEnd) def value: Int = a
  }

  case class Artist(
    name: String,
    genre: Genre,
    origin: Location,
    yearsActiveStart: YearsActiveStart,
    isActive: Boolean,
    yearsActiveEnd: YearsActiveEnd
  )
}

import model._

def searchArtists(
  artists: List[Artist],
  genres: List[String],
  locations: List[String],
  searchByActiveYears: Boolean,
  activeAfter: Int,
  activeBefore: Int
): List[Artist] = {
  artists.filter(artist => {
    (genres.isEmpty || genres.contains(artist.genre.name)) &&
    (locations.isEmpty || locations.contains(artist.origin.name)) &&
    (!searchByActiveYears || ((artist.isActive ||
      artist.yearsActiveEnd.value >= activeAfter) &&
      (artist.yearsActiveStart.value <= activeBefore)
    ))
  })
}

val artists: List[Artist] = List(
  Artist("Metallica", Genre("Heavy Metal"), Location("U.S."), YearsActiveStart(1981), true, YearsActiveEnd(0)),
  Artist("Led Zeppelin", Genre("Hard Rock"), Location("England"), YearsActiveStart(1968), false, YearsActiveEnd(1980)),
  Artist("Bee Ges", Genre("Pop"), Location("England"), YearsActiveStart(1958), false, YearsActiveEnd(2003))
)

searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022)
searchArtists(artists, List.empty, List("England"), true, 1950, 2022)
searchArtists(artists, List.empty, List.empty, true, 1950, 1979)
searchArtists(artists, List.empty, List.empty, true, 1981, 1984)
