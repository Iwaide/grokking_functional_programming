import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.{Pure, Stream}
import fs2.Stream

def castTheDie(): IO[Int] = {
  IO.delay(ch08_CastingDieImpure.NoFailures.castTheDieImpure())
}
val infiniteDieCasts: Stream[IO, Int] = Stream.eval(castTheDie()).repeat

val threeOdds = infiniteDieCasts.filter(_ % 2 == 1).take(3).compile.toList

val firstFive = infiniteDieCasts.take(5).map(i => {
  if (i == 6) i * 2 else i
}).compile.toList


val threeTotal = infiniteDieCasts.take(3).fold(0)((x, y) => x + y).compile.toList
val twoDiesAfterFive = infiniteDieCasts.find(_ == 5).append(infiniteDieCasts.take(2)).compile.toList
val hundredDies = infiniteDieCasts.take(100).compile.toList
val threeDieDoubleDie = infiniteDieCasts.take(3).append(infiniteDieCasts.take(3).map(_ * 3)).compile.toList
val doubleSix = infiniteDieCasts.find(_ == 6).append(infiniteDieCasts.find(_ == 6)).compile.drain