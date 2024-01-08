import cats.effect.IO
import cats.effect.unsafe.implicits.global

object recovery_Die {
  val dieIO = IO.delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure()).orElse(IO.pure(0))
  val cardOrDieIO = IO.
    delay(ch08_CastingDieImpure.drawAPointCard()).
    orElse(IO.delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure())).
    orElse(IO.pure(0))
  val dieTwiceIO = IO.
    delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure()).
    orElse(IO.delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure())).
    orElse(IO.pure(0))
  
  val diceAndCard = for {
      diceNum <- IO.
        delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure()).
        orElse(IO.pure(0))
      cardNum <- IO.delay(ch08_CastingDieImpure.drawAPointCard()).
      orElse(IO.pure(0))
    } yield diceNum + cardNum
  val cardAndTwoDice ={
    for {
      card <- IO.delay(ch08_CastingDieImpure.drawAPointCard())
      dice1 <- IO.delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure())
      dice2 <- IO.delay(ch08_CastingDieImpure.WithFailures.castTheDieImpure())
    } yield card + dice1 + dice2
  }.orElse(IO.pure(0))
}
