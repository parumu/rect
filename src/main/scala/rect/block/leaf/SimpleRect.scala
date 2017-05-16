package rect.block.leaf

import rect.block.{Cells, CellsRect}

case class SimpleRect(
  cells: Cells,
  override val optName: Option[String] = None,
  tags: Map[String,String] = Map.empty
) extends CellsRect
