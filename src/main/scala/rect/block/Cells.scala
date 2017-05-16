package rect.block

import rect.Yx.Yx

import scala.collection.mutable.{Map => MutMap}

case class Cells(
  xs: Seq[Seq[RectCell]],
  private val transposed: Boolean = false,
  private val ovrDim: Option[Yx] = None,
  private val offset: Yx = Yx.zero
) {
  private val actDim: Yx =
    ovrDim match {
      case None =>
        val y = xs.length
        val x = if (y == 0) 0 else xs.head.length
        Yx(y, x)
      case Some(x) => x
    }

  val dim = if (transposed) actDim.transpose else actDim

  private val m = {
    var m = MutMap[(Int,Int),RectCell]()
    var (y, x) = (0, 0)
    xs.foreach(row => {
      x = 0
      row.foreach(col => {
        m += ((y, x) -> col)
        x += 1
      })
      y += 1
    })
    m
  }

  def get(y: Int, x: Int): RectCell = {
    val(yy, xx) = if (transposed) (x, y) else (y, x)
    m(yy + offset.y, xx + offset.x)
  }

  def hHeadImpl(repeat: Int): Cells =
    this.copy(ovrDim = Some(Yx(actDim.y, repeat)))

  def hTailImpl(repeat: Int): Cells =
    this.copy(
      ovrDim = Some(Yx(actDim.y, actDim.x - repeat)),
      offset = offset + Yx(0, repeat)
    )

  def vHeadImpl(repeat: Int): Cells =
    this.copy(ovrDim = Some(Yx(repeat, actDim.x)))

  def vTailImpl(repeat: Int): Cells =
    this.copy(
      ovrDim = Some(Yx(actDim.y - repeat, actDim.x)),
      offset = offset + Yx(repeat, 0)
    )

  def hHead(repeat: Int = 1): Cells =
    if (transposed) vHeadImpl(repeat)
    else hHeadImpl(repeat)

  def hTail(repeat: Int = 1): Cells =
    if (transposed) vTailImpl(repeat)
    else hTailImpl(repeat)

  def vHead(repeat: Int = 1): Cells =
    if (transposed) hHeadImpl(repeat)
    else vHeadImpl(repeat)

  def vTail(repeat: Int = 1): Cells =
    if (transposed) hTailImpl(repeat)
    else vTailImpl(repeat)

  def transpose: Cells = this.copy(transposed = !transposed)
}

object Cells {
  val empty = Cells(Seq.empty)
  def of(xs: Seq[Seq[RectCell]]): Cells = Cells(xs)
  def ofHList(xs: RectCell*): Cells = Cells(Seq(xs))
  def ofVList(xs: RectCell*): Cells = Cells(xs.map(Seq(_)))
}

