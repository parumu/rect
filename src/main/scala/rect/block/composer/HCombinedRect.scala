package rect.block.composer

import rect.Yx.Yx
import rect.block.leaf.PadRect
import rect.block.{Rect, RectCell}

case class HCombinedRect(
  left: Rect,
  right: Rect,
  private val ovrTags: Option[Map[String,String]] = None
) extends Rect {

  // top left of right rect will be aligned to top right of left rect e.g.
  // +----+-------+
  // |left| right |
  // +----+       |
  //      |       |
  //      +-------+

  // should set optRelLoc for each child
  override val children: List[Rect] = List(left, right)

  private val (padLeft, padRight) =
    if (left.getDim.y == right.getDim.y) (left, right)
    else if (left.getDim.y < right.getDim.y) {
      val pad = PadRect(width = left.getDim.x, height = right.getDim.y - left.getDim.y)
      (left / pad, right)
    }
    else {
      val pad = PadRect(width = right.getDim.x, height = left.getDim.y - right.getDim.y)
      (left, right / pad)
    }

  val leftDim = padLeft.getDim
  val rightDim = padRight.getDim

  val dim = Yx(leftDim.y, leftDim.x + rightDim.x)

  left.optRelLoc = Some(Yx.zero)
  right.optRelLoc = Some(Yx(0, leftDim.x))

  override val tags = ovrTags match {
    case None => left.tags ++ right.tags
    case Some(x) => x
  }

  override def getRectFor(yx: Yx): Rect = {
    if (!yx.isWithin(dim)) throwOutOfRangeException(yx)

    if (yx.isWithin(leftDim)) left.getRectFor(yx)
    else right.getRectFor(yx - Yx(0, leftDim.x))
  }

  def getDim: Yx = dim

  def getCell(y: Int, x: Int): RectCell = {
    if (x < leftDim.x) padLeft.getCell(y, x)
    else padRight.getCell(y, x - leftDim.x)
  }

  def vHead(repeat: Int): Rect = padLeft.vHead(repeat) + padRight.vHead(repeat)
  def vTail(repeat: Int): Rect = padLeft.vTail(repeat) + padRight.vTail(repeat)

  def hHead(repeat: Int): Rect =
    if (repeat <= leftDim.x) padLeft.hHead(repeat)
    else padLeft + padRight.hHead(repeat - leftDim.x)

  def hTail(repeat: Int): Rect =
    if (repeat == leftDim.x) padRight
    else if (repeat > leftDim.x) padRight.hTail(repeat - leftDim.x)
    else padLeft.hTail(repeat) + padRight

  def withNewTags(tags: Map[String,String]): Rect = this.copy(ovrTags = Some(tags))

  def as2DimSeq: Seq[Seq[RectCell]] =
    (0 until dim.y).map(y =>
      (0 until dim.x).map(x =>
      getCell(y, x)))

  override def decompose: Seq[(Yx,Rect)] =
    left.decompose ++ right.decompose.map(x => (x._1 + right.optRelLoc.get, x._2))
}

