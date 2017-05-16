package rect.block.leaf

import rect.block.{Cells, CellsRect}

case class EmptyRect(
  override val tags: Map[String,String] = Map.empty
) extends CellsRect {
  val cells: Cells = Cells(Seq.empty)
}
