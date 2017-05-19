package test

import org.apache.poi.ss.usermodel.{CellStyle, Workbook}
import rect.renderer._

object SomeCellStyleBuilder extends CellStyleBuilder {
  def build(wb: Workbook): Map[CellStyleKey,CellStyle] = {
    Map(
      Normal -> baseStyle(wb),
      Bold -> baseStyle(wb, bold = true),
      Num -> formatted(wb, "[Black]#,##0;[Red]-#,##0;0"),
      YYYY_MM_DD -> formatted(wb, "yyyy-MM-dd"),
      YYYYMMDD -> formatted(wb, "yyyyMMdd")
    )
  }
}
