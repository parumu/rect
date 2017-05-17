package rect.generator

import rect.block.Rect

trait RectGen {
  val tags: Map[String,String] = Map.empty
  val optConds: Option[List[Cond]] = None
  val optValFlts: Option[List[ValFilter]] = None

  def consume(inVus: List[ValUnit]): Unit
  def generate: Rect

  def applyConds(vu: ValUnit): Option[ValUnit] =
    optConds match {
      case None => Some(vu)
      case Some(conds) =>
        conds.foldLeft(Some(vu): Option[ValUnit])((acc, cond) => cond.apply(acc))
    }

  def applyValFltrs(inVu: ValUnit): ValUnit =
    optValFlts match {
      case None => inVu
      case Some(valFlts) =>
        valFlts.foldLeft(inVu)((acc, valFlt) => valFlt.apply(acc))
    }
}
