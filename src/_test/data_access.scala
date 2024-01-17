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

  test("data access layer should fetch attractions from a real SPARQL server") {
    val result: List[Attraction] = testServerConnection
      .use(connection => {
        // given a real external data source with attractions in Venice
        val dataAccess = getSparqlDataAccess(execQuery(connection))

        // when we use it to find attractions named "Bridge of Sighs"
        dataAccess.findAttractions("Bridge of Sighs", ByLocationPopulation, 5)
      })
      .unsafeRunSync()

    // then we get a list of results with Bridge of Sighs in it
    assert(result.exists(_.name == "Bridge of Sighs") && result.size <= 5)
  }
}