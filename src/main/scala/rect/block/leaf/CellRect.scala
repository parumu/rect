package rect.block.leaf

import rect.block.{Cells, NumCell, RectCell, StrCell}
import rect.renderer.{Normal, Num}

object CellRect {
  def ofStr(x: String, style: CellStyleKey = Normal) =
    SimpleRect(Cells.ofHList(RectCell(typ = StrCell, str = x, style = style)))

  def ofNum(x: Double, style: CellStyleKey = Num) =
   SimpleRect(Cells.ofHList(RectCell(typ = NumCell, num = x, style = style)))
}
