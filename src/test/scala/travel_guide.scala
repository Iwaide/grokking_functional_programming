import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.implicits._
import ch11_TravelGuide._
import ch11_TravelGuide.model._
import ch11_TravelGuide.model.PopCultureSubject._
import ch11_TravelGuide.AttractionOrdering._
import ch11_TravelGuide.Version3.travelGuide
import ch11_WikidataDataAccess.getSparqlDataAccess
import ch12_TravelGuide.{SearchReport, Version4, Version5}
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck._, Arbitrary._, org.scalatestplus.scalacheck._
import org.apache.jena.fuseki.main.FusekiServer
import org.apache.jena.query.DatasetFactory
import org.apache.jena.rdfconnection.RDFConnection
import org.apache.jena.riot.RDFDataMgr

/** @see [[ch11_TravelGuide]] to verify requirements
  */
class ch12_TravelGuideTest extends AnyFunSuite with ScalaCheckPropertyChecks {
  /**  STEP 3b: test side effects without using any mocking libraries
    *  - testing using a real SPARQL server
    */
  def localSparqlServer: Resource[IO, FusekiServer] = {
    val start: IO[FusekiServer] = IO.blocking {
      val model  = RDFDataMgr.loadModel(getClass.getResource("testdata.ttl").toString)
      val ds     = DatasetFactory.create(model)
      val server = FusekiServer.create.add("/test", ds).build
      server.start()
      server
    }

    Resource.make(start)(server => IO.blocking(server.stop()))
  }

  val testServerConnection: Resource[IO, RDFConnection] = for {
    localServer <- localSparqlServer
    connection  <- ch12_TravelGuide.connectionResource(localServer.serverURL(), "test")
  } yield connection

  test("data access layer should fetch attractions sorted by name") {
    val attractions: List[Attraction] = testServerConnection.use(connection => {
      val dataAccess = getSparqlDataAccess(execQuery(connection))
      dataAccess.findAttractions("National Park", ByName, 5)
    }).unsafeRunSync()
    assert(attractions.size == 5 &&
      attractions.map(_.name) == attractions.sortBy(_.name).map(_.name)
    )
  }

  val veniceId: LocationId = LocationId("Q641")

  test("data access layer should fetch artists from a real SPARQL server") {
    val artists: List[Artist] = testServerConnection.use(connection => {
      val dataAccess = getSparqlDataAccess(execQuery(connection))
      dataAccess.findArtistsFromLocation(veniceId, 5)
    }).unsafeRunSync()
    assert(artists.exists(_.name == "Talco"))
  }

  test("data access layer should fetch movies from a real SPARQL server") {
    val movies: List[Movie] = testServerConnection.use(connection => {
      val dataAccess = getSparqlDataAccess(execQuery(connection))
      dataAccess.findMoviesAboutLocation(veniceId, 2)
    }).unsafeRunSync()
    assert(
      List("Spider-Man: Far from Home", "Casino Royale").forall(movieName => {
        movies.exists(_.nme == movieName)
      })
    a)
  }
}
