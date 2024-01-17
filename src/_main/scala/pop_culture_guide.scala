import javax.tools.DocumentationTool.Location
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.effect.implicits._

object ch11_TravelGuide {
  object model {
    opaque type LocationId = String
    object LocationId {
      def apply(value: String): LocationId = value
      extension (a: LocationId) def value: String = a
    }

    case class Location(id: LocationId, name: String, population: Int)
    case class Attraction(name: String, description: Option[String], location: Location)

    enum PopCultureSubject {
      case Artist(name: String, followers: Int)
      case Movie(name: String, boxOffice: Int)
    }
    case class TravelGuide(attraction: Attraction, subjects: List[PopCultureSubject])

    enum AttractionOrdering {
      case ByLocationPopulation
    }
    trait DataAccess {
      def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = ???
      def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[PopCultureSubject.Artist]] = ???
      def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[PopCultureSubject.Movie]] = ???
    }
  }
  import model._, model.PopCultureSubject._

  def guideScore(guide: TravelGuide): Int = {
      val descriptionScore = guide.attraction.description.map(_ => 30).getOrElse(0)
      val quantityScore    = Math.min(40, guide.subjects.size * 10)

      val totalFollowers: Long = guide.subjects
        .map(_ match {
          case Artist(_, followers) => followers.toLong
          case _                    => 0
        })
        .sum
      val totalBoxOffice: Long = guide.subjects
        .map(_ match {
          case Movie(_, boxOffice) => boxOffice.toLong
          case _                   => 0
        })
        .sum

      val followersScore = Math.min(15, totalFollowers / 100_000).toInt
      val boxOfficeScore = Math.min(15, totalBoxOffice / 10_000_000).toInt
      descriptionScore + quantityScore + followersScore + boxOfficeScore
    }

  def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
    for {
      attractions <- data.findAttractions(attractionName, AttractionOrdering.ByLocationPopulation, 3)
      guides <- attractions.map(
        attraction => {
        List(
          data.findArtistsFromLocation(attraction.location.id, 2),
          data.findMoviesAboutLocation(attraction.location.id, 2)
        ).parSequence.map(_.flatten).
          map(popCultureSubjects => TravelGuide(attraction, popCultureSubjects))
      }).parSequence
    } yield guides.sortBy(guideScore).reverse.headOption
  }
}

// def cachedExecQuery(connection: RDFConnection, cache: Ref[IO, Map[String, List[QuerySolution]]])(query: String): IO[List[QuerySolution]] = {
//   for {
//     cachedQueries <- cache.get
//     solutions <- cachedQueries match {
//       case Some(cachedSolutions) => IO.pure(cachedSolutions)
//       case None =>
//         for {
//           realSolutions <- execQuery(connection)(query)
//           _ <- cache.update(_.updated(query, realSolutions))
//         } yield realSolutions
//     }
//   } yield solutions
// }