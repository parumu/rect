package rect.block.composer

import rect.Yx.Yx
import rect.block.leaf.{HBarRect, SimpleRect, VBarRect}
import rect.block.{Cells, Rect, RectCell, StrCell}

// consists of 3 rects
case class ChartRect(
  vKeys: Seq[String],
  hKeys: Seq[String],
  valMap: Map[(String,String),RectCell],
  topLeft: String = "",
  override val optName: Option[String] = None,
  override val tags: Map[String,String] = Map.empty
) extends Rect {

  def makeHdr(s: String): RectCell = RectCell(typ = StrCell, str = s, style = Bold)

  val top: Rect = HBarRect.ofRectCell(
    RectCell(typ = StrCell, str = topLeft, style = Bold) +: hKeys.map(makeHdr):_*)

  val left: Rect = VBarRect.ofStr(vKeys:_*)

  val inner: Rect = {
    val cells = Cells.of(
      vKeys.map(v =>
        hKeys.map(h => {
          valMap.getOrElse((v, h), RectCell.empty)
        })
      ))
    val name = optName.map(name => s"${name}Inner")
    val rect = SimpleRect(cells, optName = name)
    rect
  }

  // should set optRelLoc for each child
  override val children = List(top, left, inner)

  private val rect = {
    val rect = top / (left + inner)
    // optRelLoc needs to be overridden
    top.optRelLoc = Some(Yx.zero)
    left.optRelLoc = Some(Yx(1, 0))
    inner.optRelLoc = Some(Yx(1, 1))
    rect
  }

  val dim = Yx(left.getDim.y + 1, top.getDim.x)

  override def getRectFor(yx: Yx): Rect = {
    if (!yx.isWithin(dim)) throwOutOfRangeException(yx)
    else if (yx.y == 0) top
    else if (yx.x == 0) left
    else inner
  }

  def getDim: Yx = dim

  def getCell(y: Int, x: Int): RectCell = {
    if (y == 0) top.getCell(y, x)
    else if (x == 0) left.getCell(y - 1, x)
    else inner.getCell(y - 1, x - 1)
  }

  def vHead(repeat: Int): Rect = rect.vHead(repeat)
  def hHead(repeat: Int): Rect = rect.hHead(repeat)
  def vTail(repeat: Int): Rect = rect.vTail(repeat)
  def hTail(repeat: Int): Rect = rect.hTail(repeat)

  def withNewTags(tags: Map[String,String]): Rect = this.copy(tags = tags)

  def as2DimSeq: Seq[Seq[RectCell]] =
    (0 until dim.y).map(y =>
      (0 until dim.x).map(x =>
        getCell(y, x)))

  override def decompose: Seq[(Yx,Rect)] =
    children.map(x => (x.optRelLoc.get, x))
}




