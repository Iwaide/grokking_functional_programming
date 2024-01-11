import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import scala.jdk.CollectionConverters._

object ch09_Stream {
  object model {
    opaque type Currency = String
    object Currency {
      def apply(name: String): Currency               = name
      extension (currency: Currency) def name: String = currency
    }
  }
  import model._

  def extractSingleCurrencyRate(currencyToExtract: Currency)
    (table: Map[Currency, BigDecimal]): Option[BigDecimal] = {
      // var rateOption = table.find(ratePair => {
      //   ratePair match {
      //     case (currency, _) => currency == currencyToExtract
      //   }
      // })
      // rateOption.map(rate => rate match {
      //   case (_, rate) => rate
      // })
      table.get(currencyToExtract)
    }

  def currencyRate(from: Currency, to: Currency): IO[BigDecimal] = {
    val rateMapIO = exchangeTable(from)
    for {
      rateMap <- rateMapIO
      result <- extractSingleCurrencyRate(to)(rateMap) match {
        case Some(rate) => IO.pure(rate)
        case None => currencyRate(from, to)
      }
    } yield result
  }

  def exchangeTable(from: Currency): IO[Map[Currency, scala.math.BigDecimal]] = {
    IO.delay(ch09_CurrencyExchangeImpure.exchangeRatesTableApiCall(from.name)).map(table =>
      table.asScala.toMap.map { case (currencyName, javaRate) =>
        (Currency(currencyName), scala.math.BigDecimal(javaRate))
      }
    )
  }
}

// import model._

// val usdExchangeTables = List(
//   Map(Currency("EUR") -> BigDecimal(0.88)),
//   Map(
//     Currency("EUR") -> BigDecimal(0.89),
//     Currency("JPY") -> BigDecimal(114.62)
//   ),
//   Map(Currency("JPY") -> BigDecimal(114))
// )
// usdExchangeTables.map(extractSingleCurrencyRate(Currency("EUR")))
// usdExchangeTables.map(extractSingleCurrencyRate(Currency("JPY")))
