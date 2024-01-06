// object model {
//   opaque type Artist = String
//   object Artist {
//     def apply(value: String): Artist = value
//   }

//   opaque type User = String
//   object User {
//     def apply(value: String): User = value
//   }

//   case class Song(
//     artist: Artist,
//     name: String
//   )

//   enum MusicGenre {
//     case Electronica
//     case DrumnBass
//     case Ambient
//   }

//   enum PlaylistCategory {
//     case UserGenerated(user: User)
//     case ArtistRelated(artist: Artist)
//     case GenreGroup(genres: Set[MusicGenre])
//   }

//   case class Playlist(
//     name: String,
//     category: PlaylistCategory,
//     songs: List[Song]
//   )

// }

// import model._, model.MusicGenre._, model.PlaylistCategory._

// val fooFighters = Artist("Foo Fighters")
// val playlist = Playlist(
//   name: "This is Foo Fighters",
//   category: ArtistRelated(fooFighters),
//   songs: List(
//     Song(fooFighters, "Breakout"),
//     Song(fooFighters, "Learn To Fly")
//   )
// )

object model {
  opaque type User = String
  object User {
    def apply(name: String): User = name
  }

  opaque type Artist = String
  object Artist {
    def apply(name: String): Artist = name
  }

  case class Song(artist: Artist, title: String)

  enum MusicGenre {
    case House
    case Funk
    case HipHop
  }

  enum PlaylistKind {
    case CuratedByUser(user: User)
    case BasedOnArtist(artist: Artist)
    case BasedOnGenres(genres: Set[MusicGenre])
  }

  case class Playlist(
    name: String,
    kind: PlaylistKind,
    songs: List[Song]
  )
}

import model._, model.MusicGenre._, model.PlaylistKind._

val fooFighters = Artist("Foo Fighters")
val playlist1 = Playlist(
  "This is the Foo Fighters",
  BasedOnArtist(fooFighters),
  List(
    Song(fooFighters, "Breakout"),
    Song(fooFighters, "Learn To Fly")
  )
)
val daftPunk = Artist("Daft Punk")
val chemicalBrothers = Artist("Chemical Brothers")

val playlist2 = Playlist(
  "Deep Focus",
  BasedOnGenres(Set(
    House, Funk
  )),
  List(
    Song(daftPunk, "One More Time"),
    Song(chemicalBrothers, "Hey Boy, Hey Girl")
  )
)

def gatherSongs(
  playlists: List[Playlist],
  artist: Artist,
  genre: MusicGenre
): List[Song] = {
  playlists.flatMap(playlist => {
    playlist.kind match {
      case CuratedByUser(_) => playlist.songs.filter(song => song.artist == artist)
      case BasedOnArtist(basedArtist) => if(artist == basedArtist) playlist.songs else List.empty
      case BasedOnGenres(genres) => if (genres.contains(genre)) playlist.songs else List.empty
    }
  })
}

gatherSongs(List(playlist1, playlist2), fooFighters, House)
