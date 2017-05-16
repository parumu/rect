package rect.generator

import java.time.LocalDate

case class ValUnit(
  date: LocalDate,
  v: Double,
  typ: Option[String] = None,
  ccy: Option[String] = None,
  book: Option[String] = None,
  tradeId: Option[String] =None,
  attrs: Map[String,String] = Map.empty
) {
  override def toString = List(date, tradeId, typ, book, attrs, v).mkString(",")
}
