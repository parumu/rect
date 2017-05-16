package rect.block

import rect.Yx.Yx
import rect.block.leaf.SimpleRect

abstract class CellsRect extends Rect {
  protected val cells: Cells
  def getDim: Yx = cells.dim
  def getCell(y: Int, x: Int): RectCell = cells.get(y, x)
  def as2DimSeq: Seq[Seq[RectCell]] = cells.xs

  def vHead(repeat: Int = 1): Rect = SimpleRect(cells.vHead(repeat))
  def hHead(repeat: Int = 1): Rect = SimpleRect(cells.hHead(repeat))
  def vTail(repeat: Int = 1): Rect = SimpleRect(cells.vTail(repeat))
  def hTail(repeat: Int = 1): Rect = SimpleRect(cells.hTail(repeat))
  def withNewTags(tags: Map[String,String]) = SimpleRect(cells, tags = tags)
}
