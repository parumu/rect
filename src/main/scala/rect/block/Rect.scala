package rect.block

import rect.Yx.Yx
import rect.block.composer.{HCombinedRect, VCombinedRect}
import rect.block.leaf.SimpleRect
import rect.renderer.CellStyleKey

import scala.collection.mutable.{Map => MutMap}

abstract class Rect {
  val tags: Map[String,String]
  def getDim: Yx
  def getCell(y: Int, x: Int): RectCell
  def getRectFor(yx: Yx): Rect = this
  def throwOutOfRangeException(coord: Yx) = throw new IllegalArgumentException(
    s"Rect's dim is $getDim, but tried to access $coord")

  def +(that: Rect): Rect = HCombinedRect(this, that)
  def /(that: Rect): Rect = VCombinedRect(this, that)

  def vHead(repeat: Int = 1): Rect
  def hHead(repeat: Int = 1): Rect
  def vTail(repeat: Int = 1): Rect
  def hTail(repeat: Int = 1): Rect

  def decompose: Seq[(Yx,Rect)] = Seq((Yx.zero, this))

  def vKeyTotal(
    optKeyStyle: Option[CellStyleKey] = None,
    optValStyle: Option[CellStyleKey] = None
  ): Rect = {
    val key2Totals = MutMap[String,Array[Double]]()

    (0 until getDim.y).foreach(y => {
      val key = getCell(y, 0).getStrVal
      val totals = key2Totals.get(key) match {
        case None =>
          val totals = Array.fill[Double](getDim.x - 1)(0)
          key2Totals += (key -> totals)
          totals
        case Some(x) => x
      }
      (1 until getDim.x).foreach(x => totals(x - 1) += getCell(y, x).getNumVal)
    })
    val cells = Cells.of(key2Totals.toSeq.sortBy(_._1).map {
      case (key, totals) =>
        val keyStyle = optKeyStyle.orNull
        val valStyle = optValStyle.orNull
        RectCell(typ = StrCell, str = key, style = keyStyle) +:
          totals.map(x => RectCell(typ = NumCell, num = x, style = valStyle)).toSeq
    })
    SimpleRect(cells)
  }

  def vTotal(optStyle: Option[CellStyleKey] = None): Rect = {
    val totals = Array.fill[Double](getDim.x)(0)
    (0 until getDim.y).foreach(y => {
      (0 until getDim.x).foreach(x => {
        totals(x) += getCell(y, x).getNumVal
      })
    })
    val style = optStyle.orNull
    SimpleRect(Cells.of(Seq(totals.map(x => RectCell(typ = NumCell, num = x, style = style)).toSeq)))
  }
  def as2DimSeq: Seq[Seq[RectCell]]
  def withNewTags(tags: Map[String,String]): Rect
  val children: List[Rect] = List.empty
  val optName: Option[String] = None
  var optRelLoc: Option[Yx] = None
}