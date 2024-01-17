import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import cats.effect.implicits._
import fs2.{Pure, Stream}
import cats.effect.Ref
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt
import scala.runtime.Statics

def castTheDie(): IO[Int] = {
  IO.delay(ch08_CastingDieImpure.NoFailures.castTheDieImpure())
}

// val hoge =
//   for {
//     counter <- Ref.of[IO, Int](0)
//     _ <- IO.sleep(FiniteDuration(1, TimeUnit.SECONDS))
//     die1 = castTheDie().flatMap(i => counter.update(_ + i))
//     die2 = castTheDie().flatMap(i => counter.update(_ + i))
//     _ <- List(die1, die2).parSequence
//     result <- counter.get
//   } yield result

val twoDiesTotal =
  for {
    _ <- IO.sleep(1.second)
    result <- List(castTheDie(), castTheDie()).parSequence
  } yield result.sum

// val twoDiesList = 
//   for {
//     ref <- Ref.of[IO, List[Int]](List.empty)
//     list <- List(castTheDie(), castTheDie()).parSequence
//     _ <- ref.update(_.appendedAll(list))
//     result <- ref.get
//   } yield result

val twoDiesList =
  for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast = castTheDie().flatMap(result => storedCasts.update(_.appended(result)))
    _ <- List(singleCast, singleCast).parSequence
    result <- storedCasts.get
  } yield result

val threeDiesList =
  for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast = castTheDie().flatMap(result => storedCasts.update(_.appended(result)))
    _ <- List.fill(3)(singleCast).parSequence
    result <- storedCasts.get
  } yield result

val sixDiesCount =
  for {
    counter <- Ref.of[IO, Int](0)
    countSix = castTheDie().flatMap(result => if (result == 6) counter.update(_ + 1) else IO.unit)
    _ <- List.fill(100)(countSix).parSequence
    result <- counter.get
  } yield result

val hundredDiesTotal = List.fill(100)(IO.sleep(1.second).
  flatMap(_ => castTheDie())).parSequence.map(_.sum)