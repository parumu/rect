package rect.generator

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import rect.renderer.{Num, YYYY_MM_DD}
import util.DateUtil

sealed trait ItemType
case object TotalItem extends ItemType
case object AttrItem extends ItemType

trait AxisItem {
  val name: String
  val typ: ItemType
  val style: Option[CellStyleKey]
  val selector: Option[List[String]]
  def getVal(vu: ValUnit): String
}

abstract class AttrAxisItem extends AxisItem {
  val typ: ItemType = AttrItem
  val style: Option[CellStyleKey] = None
  val selector: Option[List[String]] = None
  def getVal(vu: ValUnit): String
}

case class Const(
  name: String = "",
  v: String = "",
  style: Option[CellStyleKey] = None,
  selector: Option[List[String]] = None
) extends AxisItem {
  val typ = AttrItem
  def getVal(vu: ValUnit): String = v
}

abstract class Attr(
  name: String,
  attr: String,
  typ: ItemType,
  style: Option[CellStyleKey] = None,
  selector: Option[List[String]] = None
) extends AxisItem {
  def getVal(vu: ValUnit): String = vu.attrs.getOrElse(attr, "")
}

case class StrAttr(
  name: String,
  attr: String,
  typ: ItemType = AttrItem,
  style: Option[CellStyleKey] = None,
  selector: Option[List[String]] = None
) extends Attr(name, attr, typ, style)

case class NumAttr(
  name: String,
  attr: String,
  typ: ItemType = AttrItem,
  style: Option[CellStyleKey] = Some(Num),
  selector: Option[List[String]] = None
) extends Attr(name, attr, typ, style)

case class DateAttr(
  name: String,
  attr: String,
  outAsStr: Boolean = true,
  typ: ItemType = AttrItem,
  style: Option[CellStyleKey] = Some(YYYY_MM_DD),
  selector: Option[List[String]] = None,
  strFmt: DateTimeFormatter = DateUtil.formatters.yyyy_MM_dd
) extends Attr(name, attr, typ, style) {
  def tryParseAsDate(s: String): Option[LocalDate] =
    if (s == "") None else Some(LocalDate.parse(s, DateUtil.formatters.yyyy_MM_dd))
}

case class Date(
  name: String,
  typ: ItemType = AttrItem,
  outAsStr: Boolean = true,
  style: Option[CellStyleKey] = Some(YYYY_MM_DD),
  selector: Option[List[String]] = None,
  strFmt: DateTimeFormatter = DateUtil.formatters.yyyy_MM_dd
) extends AxisItem {
  def getVal(vu: ValUnit): String = strFmt.format(vu.date)
}

case class Total(
  name: String = Total.defaultName,
  style: Option[CellStyleKey] = None,
  selector: Option[List[String]] = Some(List(Total.defaultName))
) extends AxisItem {
  val valKey = selector match {
    case None | Some(Nil) => name
    case Some(x :: rest) => x
  }
  val typ = TotalItem
  def getVal(vu: ValUnit): String = valKey
}

object Total {
  val defaultName = "Total"
}














