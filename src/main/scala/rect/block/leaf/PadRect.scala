package rect.block.leaf

import rect.block.{Cells, CellsRect, RectCell}

case class PadRect(
  width: Int = 1,
  height: Int = 1,
  filler: RectCell = RectCell.empty,
  override val tags: Map[String,String] = Map.empty
) extends CellsRect {
  val cells: Cells = Cells.of((1 to height).map(_ => (1 to width).map(_ => filler)))
}
