package rect.renderer

import org.apache.poi.ss.usermodel.{CellStyle, Workbook}

trait CellStyleKey // not sealed so that user can extend
case object Normal extends CellStyleKey
case object Bold extends CellStyleKey
case object Num extends CellStyleKey
case object YYYY_MM_DD extends CellStyleKey
case object YYYYMMDD extends CellStyleKey

trait CellStyleBuilder {
  def build(wb: Workbook): Map[CellStyleKey,CellStyle]

  def baseStyle(wb: Workbook, bold: Boolean = false): CellStyle = {
    val font = {
      val x = wb.createFont
      x.setFontHeightInPoints(10)
      x.setFontName("Arial")
      x.setBold(bold)
      x
    }
    val style = wb.createCellStyle
    style.setFont(font)
    style
  }

  def formatted(wb: Workbook, fmt: String, bold: Boolean = false): CellStyle = {
    val x = baseStyle(wb, bold)
    x.setDataFormat(wb.createDataFormat.getFormat(fmt))
    x
  }
}
