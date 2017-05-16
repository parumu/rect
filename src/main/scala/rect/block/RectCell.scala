package rect.block

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import rect.renderer.Num
import util.DateUtil

abstract class CellType
case object NumCell extends CellType
case object StrCell extends CellType
case object DateCell extends CellType
case object FormulaCell extends CellType
case object EmptyCell extends CellType

// optimized for speed
case class RectCell(
  typ: CellType,
  num: Double = 0.0,
  str: String = null,
  date: LocalDate = null,
  style: CellStyleKey = null,
  dateFmt: DateTimeFormatter  = DateUtil.formatters.yyyy_MM_dd
) extends Ordered[RectCell] {

  if (typ == StrCell && str == null) throw new IllegalArgumentException("Str rect cell is missing str")
  if (typ == DateCell && date == null) throw new IllegalArgumentException("Date rect cell is missing date")
  if (typ == FormulaCell && str == null) throw new IllegalArgumentException("Formula rect cell is missing str")

  def getStrVal: String = str
  def getNumVal: Double = num
  def getDateVal: LocalDate = date
  def getFormula: String = str
  def getStyle: CellStyleKey = style

  def +(that: RectCell): RectCell = {
    val mergedStyle = mergeStyle(style, that.getStyle)

    if (typ == that.typ) {
      if (typ == StrCell) RectCell(StrCell, str = str + that.str, style = mergedStyle)
      else if (typ == NumCell) RectCell(NumCell, num = num + that.num, style = mergedStyle)
      else this // addition not supported
    }
    else this // addition not supported
  }

  def mergeStyle(a: CellStyleKey, b: CellStyleKey): CellStyleKey = {
    if (a == null && b == null) null
    else if (a != null && b == null) a
    else if (a == null && b != null) b
    else a  // return a if in conflict
  }

  def compare(that: RectCell): Int = {
    if (typ == that.typ) {
      if (typ == NumCell) num.compareTo(that.num)
      else if (typ == StrCell) str.compareTo(that.str)
      else if (typ == DateCell) date.compareTo(that.date)
      else 0  // formula is not comparable
    }
    else 0 // unable to compare
  }
}

object RectCell {
  def zero(style: CellStyleKey = Num) = RectCell(typ = NumCell, style = style)
  val empty = RectCell(typ = EmptyCell)
}