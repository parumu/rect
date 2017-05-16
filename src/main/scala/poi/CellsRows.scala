package poi

import org.apache.poi.ss.usermodel.{Cell, Row, Sheet}

case class CellsRows(sh: Sheet) {
  def getRow(i: Int): Row = {
    val existing = sh.getRow(i)
    if (existing != null) existing
    else sh.createRow(i)
  }

  def getCell(row: Row, i: Int): Cell = {
    val existing = row.getCell(i)
    if (existing != null) existing
    else row.createCell(i)
  }
}
