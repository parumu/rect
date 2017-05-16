package rect.block.leaf

import rect.block.{Cells, Rect, RectCell, StrCell}

object VBarRect {
  def ofStr(xs: String*): Rect =
    SimpleRect(Cells.ofVList(xs.map(x => RectCell(typ = StrCell, str = x)):_*))

  def ofRectCell(xs: RectCell*): Rect = SimpleRect(Cells.ofVList(xs:_*))
}
