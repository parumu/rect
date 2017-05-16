package poi

import org.apache.poi.ss.usermodel.{Row, Sheet}

case class SheetRows(sh: Sheet) {
  def get(i: Int): Row =
    sh.getRow(i: Int) match {
      case null => sh.createRow(i)
      case row => row
    }
}
