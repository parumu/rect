package rect.block.composer

import rect.Yx.Yx
import rect.block.leaf.PadRect
import rect.block.{Rect, RectCell}

case class VCombinedRect(
  above: Rect,
  below: Rect,
  private val ovrTags: Option[Map[String,String]] = None
) extends Rect {

  // bottom left of above rect will be aligne to top left of left rect e.g.
  // +-------+
  // | above |
  // +-------+---+
  // |   below   |
  // +-----------+

  // should set optRelLoc for each child
  override val children: List[Rect] = List(above, below)

  private val (padAbove, padBelow) =
    if (above.getDim.x == below.getDim.x) (above, below)
    else if (above.getDim.x < below.getDim.x) {
      val pad = PadRect(width = below.getDim.x - above.getDim.x, height = above.getDim.y)
      (above + pad, below)
    }
    else {
      val pad = PadRect(width = above.getDim.x - below.getDim.x, height = below.getDim.y)
      (above, below + pad)
    }

  val aboveDim = padAbove.getDim
  val belowDim = padBelow.getDim

  val dim = Yx(aboveDim.y + belowDim.y, aboveDim.x)

  above.optRelLoc = Some(Yx.zero)
  below.optRelLoc = Some(Yx(aboveDim.y, 0))

  override val tags = ovrTags match {
    case None => above.tags ++ below.tags
    case Some(x) => x
  }

  override def getRectFor(yx: Yx): Rect = {
    if (!yx.isWithin(dim)) throwOutOfRangeException(yx)

    if (yx.isWithin(aboveDim)) above.getRectFor(yx)
    else below.getRectFor(yx - Yx(aboveDim.y, 0))
  }

  def getDim: Yx = dim

  def getCell(y: Int, x: Int): RectCell = {
    if (y < aboveDim.y) padAbove.getCell(y, x)
    else padBelow.getCell(y - aboveDim.y, x)
  }

  def vHead(repeat: Int): Rect =
    if (repeat <= aboveDim.y) padAbove.vHead(repeat)
    else padAbove + padBelow.vHead(repeat - aboveDim.y)

  def vTail(repeat: Int): Rect =
    if (repeat == aboveDim.y) padBelow
    else if (repeat > aboveDim.y) padBelow.vTail(repeat - aboveDim.y)
    else padAbove.vTail(repeat) + padBelow

  def hHead(repeat: Int): Rect = padAbove.hHead(repeat) / padBelow.hHead(repeat)
  def hTail(repeat: Int): Rect = padAbove.hTail(repeat) / padBelow.hTail(repeat)

  def withNewTags(tags: Map[String,String]): Rect = this.copy(ovrTags = Some(tags))

  def as2DimSeq: Seq[Seq[RectCell]] =
    (0 until dim.y).map(y =>
      (0 until dim.x).map(x =>
      getCell(y, x)))

  override def decompose: Seq[(Yx,Rect)] =
    above.decompose ++ below.decompose.map(x => (x._1 + below.optRelLoc.get, x._2))
}

