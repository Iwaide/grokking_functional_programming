import cats.effect.{IO, Ref}
import cats.implicits._
import cats.effect.unsafe.implicits.global
import fs2.Stream

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._


object model {
  opaque type City = String
  object City {
    def apply(name: String): City = name
  }
  case class CityStats(city: City, checkIns: Int)

  // def processCheckIns(checkIns: Stream[IO, City]): IO[Unit] = {
  //   checkIns.fold(Map.empty[City, Int])((acc, city) => {
  //     println(acc.toList.sortBy(_._2)(Ordering[Int].reverse))
  //     acc.updated(
  //       city,
  //       acc.getOrElse(city, 0) + 1
  //     )
  //   }).compile.drain
  // }

  def topCities(cityCheckIns: Map[City, Int]): List[CityStats] = {
    cityCheckIns.toList.map(_ match {
      case (city, checkIns) => CityStats(city, checkIns)
    }).sortBy(_.checkIns).reverse.take(3)
  }
}
import model._

val checkIns: Stream[IO, City] =
    Stream(City("Sydney"), City("Sydney"), City("Cape Town"), City("Singapore"), City("Cape Town"), City("Sydney")).
    repeatN(100_000).
    append(Stream.range(0, 100_000).map(i => City(s"City $i"))).
    append(Stream(City("Sydney"), City("Sydney"), City("Lima"))).
    covary[IO]

val hoge = checkIns.scan(Map.empty[City, Int])((cityCheckIns, city) => {
  cityCheckIns.updatedWith(city)(_.map(_ + 1).orElse(Some(1)))
  // val newCheckIns = cityCheckIns.get(city) match {
  //   case None => 1
  //   case Some(checkIns) => checkIns + 1
  // }
  // cityCheckIns.updated(city, newCheckIns)

}).
  // ここでscanが吐いたMapが10万単位でまとめられる
  chunkN(100_000).
  map(_.last).unNone.map(topCities).foreach(IO.println).compile.drain
// .map(topCities).foreach(IO.println).compile.drain

def updateRanking(
  storedCheckIns: Ref[IO, Map[City, Int]],
  storedRanking: Ref[IO, List[CityStats]]
): IO[Nothing] = {
  (for {
    newRanking <- storedCheckIns.get.map(topCities)
    _ <- storedRanking.set(newRanking)
  } yield ()).foreverM
}

def storeCheckIn(
  storedCheckIns: Ref[IO, Map[City, Int]],
)(city: City): IO[Unit] = {
  storedCheckIns.update(_.updatedWith(city)(_ match {
    case None => Some(1)
    case Some(checkIns) => Some(checkIns + 1)
  }))
}

def printRanking(storedRanking: Ref[IO, List[CityStats]]): IO[Nothing] = {
  (for {
    _ <- IO.sleep(1.second)
    ranking <- storedRanking.get
    _ <- IO.println(ranking.sortBy(_.checkIns).reverse)
  } yield ()).foreverM
}

def processCheckIns(checkIns: Stream[IO, City]): IO[Unit] = {
  for {
    storedCheckIns <- Ref.of[IO, Map[City, Int]](Map.empty)
    storedRanking <- Ref.of[IO, List[CityStats]](List.empty)
    rankingProgram = updateRanking(storedCheckIns, storedRanking)
    checkInsProgram = checkIns.evalMap(storeCheckIn(storedCheckIns)).compile.drain
    printProgram = printRanking(storedRanking)
    _ <- List(rankingProgram, checkInsProgram, printProgram).parSequence
  } yield ()
}
