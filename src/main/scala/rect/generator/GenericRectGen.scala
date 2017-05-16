package rect.generator

import org.slf4j.{Logger, LoggerFactory}
import rect.block._
import rect.block.leaf.SimpleRect
import rect.renderer._

import scala.collection.mutable.{Map => MutMap}

sealed trait Axis
case class KeyAxis(item: AxisItem) extends Axis
case class ValAxis(items: AxisItem*) exnteds Axis

case class GenericRectGen(
  vAxis: Axis,
  hAxis: Axis,
  inclHdr: Boolean = true,
  hdrBold: Boolean = true,
  optKeyLabelList: Option[List[(String,String)]] = None,
  override val optConds: Option[List[Cond]] = None,
  override val optValFlts: Option[List[ValFilter]] = None,
  override val tags: Map[String,String] = Map.empty
) extends RectGen {
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  val optKeyLabelMap = optKeyLabelList.map(_.toMap)

  // Key axis specifies a key for Val axis values
  // val axis specifies values that associate with the Key axis
  val (keyAxis: KeyAxis, valAxis: ValAxis) = (vAxis, hAxis) match {
    case (KeyAxis(_), x) if x.isInstanceOf[ValAxis] => (vAxis, hAxis)
    case (x, KeyAxis(_)) if x.isInstanceOf[ValAxis] => (hAxis, vAxis)
    case _ => throw new IllegalArgumentException("Both KeyAxis and ValAxis need to be specified")
  }

  private val attr = MutMap.empty[(String, AxisItem), RectCell]
  private val total = MutMap.empty[(String, AxisItem, String), RectCell]

  // inVus are converted to RectCell and stored in:
  // - Attr map: (KeyAxis key, ValAxisItem) -> val
  // - Total map: (KeyAxis key, ValAxisItem, val) -> total
  def consume(inVus: List[ValUnit]): Unit = {
    inVus.flatMap(applyConds).foreach(srcVu => {
      val vu = optValFlts.map(valFlts => {
        valFlts.foldLeft(srcVu)((acc, valFlt) => valFlt.apply(acc))
      }).getOrElse(srcVu)

      val keyVal = {
        val s = keyAxis.item.getVal(vu)
        optKeyLabelMap.map(_.get(s)) match {
          case Some(Some(converted) => converted)
          case _ => s
        }
      }
      valAxis.items.foreach(valAxisItem => {
        val valVal = valAxisItem.getVal(vu)

        valAxisItem.typ match {
          case TotalItem => // accumulate in total map as RectCell
            val k = (keyVal, valAxisItem, valVal)
            val base = RectCell(
              typ = NumCell, num = vu.v, style = valAxisItem.style.getOrElse(Num))
            total += k -> (base + total.getOrElse(k, RectCell.zero(
              style = valAxisItem.style.getOrElse(Num))))

          case AttrItem => // accumulate in attr map as RectCell
            val cell = valAxisItem match {
              case x: NumAttr =>
                RectCell(typ = NumCell, num = valVal.toDouble, style = x.style.getOrElse(Num))

              case x: Date =>
                if (x.outAsStr) RectCell(
                  typ = StrCell,
                  str = x.strFmt.format(vu.date),
                  style = x.style.getOrElse(Normal)
                )
                else RectCell(typ = DateCell, date = vu.date, style = x.style.getOrElse(YYYYMMDD))

              case x: DateAttr => x.tryParseAsDate(valVal) match {
                case None => RectCell.empty
                case Some(y) =>
                  if (x.outAsStr) RectCell(
                    typ = StrCell,
                    str = x.strFmt.format(y),
                    style = x.style.getOrElse(Normal)
                  )
                  else RectCell(typ = DateCell, date = y, style = x.style.getOrElse(YYYYMMDD))
              }
              case x => RectCell(typ = StrCell, str = valVal, style = x.style.getOrElse(Normal))
            }
            attr += (keyVal, valAxisItem) -> cell
        }
      })
    })

    def generate: Rect = {
      log.info(s"Started generating rect from $tags...")

      // first build cells with vAxis to be the key axis ans transpose later if needed

      log.info("Building total item to val axis keys...")
      val totalItem2ValAxisKeys = {
        val itm2SelectorKeys = valAxis.items
          .filter(_.typ == TotalItem)
          .flatMap(x => x.selector.map(x -> _))
          .toMap

        // if there is a non-Total TotalItem w/o selector
        // create keys from total map and subtract select keys from it
        // Then associate the keys to the TotalItem
        val totalItemWoSel = valAxis.items.filter(x => {
          x.typ == TotalItem &&
          !x.isInstanceOf[Total] &&
          x.selector.isEmpty
        })
        // There can't be multiple non-Total TotalItem without selector
        if (totalItemWoSel.length > 1)
          throw new IllegalStateException(
            "There exists multiple non-total TotalItem w/o selector")

        (totalItemWoSel.headOption match {
          case None => Map.empty[AxisItem,List[String]]
          case Some(itm) =>
            // build keys from totals map valVal
            val totalKeys = total.map(_._1._3).toSet
            val selectorKeys = itm2SelectorKeys.values.flatten.toSet
            Map(itm -> totalKeys.diff(selectorKeys).toList.distinct.sorted)
        }) ++ itm2SelectorKeys
      }

      val keys = {
        val accumKeys = total.map(_._1._1).toSet ++ attr.map(_._1._1).toSet

        optKeyLabelList match {
          case Some(keyLabelList) =>
            keyLabelList.map(_._2).filter(accumKeys.contains) // sort in keyLabelList order

          case None => accumKeys.toSeq.sorted
        }
      }

      // build matrix
      log.info("BUilding matrix...")

      val cells: Seq[Seq[RectCell]] = {
        val hdrAttrs: Option[CellStyleKey] = if (hdrBold) Some(Bold) else None

        val dataRows = keys.map(key =>
          RectCell(typ = StrCell, str = key, style = hdrAttrs.orNull) +: valAxis.items.flatMap(vaItem => {
            vaItem.typ match {
              case AttrItem =>
                val v = attr.getOrElse((key, vaItem), RectCell.empty)
                Seq(v)

              case TotalItem =>
                totalItem2ValAxisKeys.get(vaItem) match {
                  case Some(vaKeys) =>
                    vaKeys.map(vaKey =>
                      total.getOrElse((key, vaItem, vaKey), RectCell.zero(style = vaItem.style.orNull))
                    )
                  case None => Seq(RectCell.zero(style = vaItem.style.orNull))
                }
            }
          }))

        if (inclHdr) {
          val hdrRow = {
            val keyAxisName = keyAxis.item.name
            val valAxisNames = valAxis.items.flatMap {
              case x: Total => List(x.name)
              case x if x.typ == TotalItem =>
                totalItem2ValAxisKeys.get(x) match {
                  case Some(y) => y
                  case None => throw new IllegalStateException(
                    s"TotalItem keys are underined for $x"
                  )
                }
              case x => List(x.name)
            }
            keyAxisName +: valAxisNames
          }.map(v => RectCell(typ = StrCell, str = v, style = hdrAttrs.orNull))
          hdrRow +: dataRows
        } else dataRows
      }

      // transpose if hAxis is the key axis
      val res = SimpleRect(
        cells = {
          val xs = Cells.of(cells)
          if (hAxis == keyAxis) xs.transpose else xs
        },
        tags = tags,
        optName = tags.get("name")
      )
      log.info("Done.")
      res
    }
  }
}




















