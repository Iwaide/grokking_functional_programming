case class Artist(
  name: String,
  genre: String,
  origin: String,
  yearsActiveStart: Int,
  isActive: Boolean,
  yearsActiveEnd: Int
)

val artists: List[Artist] = List(
  Artist("Metallica", "Heavy Metal", "U.S.", 1981, true, 0),
  Artist("Led Zeppelin", "Hard Rock", "England", 1968, false, 1980),
  Artist("Bee Ges", "Pop", "England", 1958, false, 2003)
)
def searchArtists(
  artists: List[Artist],
  genres: List[String],
  locations: List[String],
  searchByActiveYears: Boolean,
  activeAfter: Int,
  activeBefore: Int
): List[Artist] = {
  artists.filter(artist => {
    // genres.forall(genre => genre == artist.genre) &&
    // locations.forall(location => location == artist.origin) &&
    // searchByActiveYears &&
    // activeAfter <= artist.yearsActiveStart &&
    // activeBefore >= artist.yearsActiveEnd
    // activeBefore >= artist.yearsActiveEnd
    (genres.isEmpty || genres.contains(artist.genre)) &&
    (locations.isEmpty || locations.contains(artist.origin)) &&
    (!searchByActiveYears || ((artist.isActive ||
      artist.yearsActiveEnd >= activeAfter) &&
      (artist.yearsActiveStart <= activeBefore)
    ))
  })
}

searchArtists(artists, List("Pop"), List("England"), true, 1950, 2022)
searchArtists(artists, List.empty, List("England"), true, 1950, 2022)
searchArtists(artists, List.empty, List.empty, true, 1950, 1979)
searchArtists(artists, List.empty, List.empty, true, 1981, 1984)
