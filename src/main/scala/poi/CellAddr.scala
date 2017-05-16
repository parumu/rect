package poi

import org.apache.poi.ss.util.CellReference

case class CellAddr(row: Int, col: Int) {
  val alphaNum = CellReference.convertNumToColString(col)

  def offset(x: (Int, Int)) = CellAddr(this.row + x._1, this.col + x._2)

  override def toString: String = s"($row, $col)[$alphaNum]"
}
