package test

import java.io.File
import java.time.LocalDate

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import rect.block._
import rect.block.BlockUtil._
import rect.block.leaf._
import rect.generator.{Total, _}
import rect.renderer.{Bold, CellStyleKey, ExcelRectRender}

import scala.util.Random

class Test1 extends AssertionsForJUnit {
  implicit val csb = SomeCellStyleBuilder

  def gen(name: String, rects: Rect*): Unit = {
    val f = new File(s"/home/kmasaki/tmp/out/$name.xlsx")
    val namedRects = rects.zipWithIndex.map {
      case (x, i) => x.withNewTags(Map("sheet" -> s"test$i"))
    }
    ExcelRectRender(f).run(namedRects:_*)
  }

  def mat(y: Int, x: Int, name: Option[String] = None): Rect = {
    val rnd = Random
    val cells = (1 to y).map(y =>
      (1 to x).map(_ => {
        RectCell(typ = NumCell, num = rnd.nextInt(100))
      }))
    SimpleRect(Cells.of(cells), optName = name)
  }

  case class FromTotal(
    name: String = "from",
    style: Option[CellStyleKey] = None,
    selector: Option[List[String]] = None
  ) extends AxisItem {
    val typ = TotalItem
    def getVal(vu: ValUnit): String = vu.attrs("from")
  }

  @Test
  def testAttrTotal() = {
    val rg = GenericRectGen(
      vAxis = KeyAxis(
        StrAttr(name = "Fruit", attr = "name")
      ),
      hAxis = ValAxis(
        FromTotal()
      )
    )

    def vu(amt: Double, name: String, from: String): ValUnit =
      ValUnit(
        date = LocalDate.now(),
        v = amt,
        attrs = Map("name" -> name, "from" -> from)
      )

    val r = rg.consume(List(
      vu(10, "Carrot", "Saitama"),
      vu(5, "Carrot", "Saitama"),
      vu(2, "Potato", "Chiba"),
      vu(50, "Potato", "Hokkaido"),
      vu(23, "Potato", "Hokkaido")
    )).generate

    gen("total", r)
  }

  @Test
  def testChartWithTotal() = {
    val rg = GenericRectGen(
      vAxis = KeyAxis(
        StrAttr(name = "Fruit", attr = "name")
      ),
      hAxis = ValAxis(
        NumAttr(name = "Price", attr = "price"),
        NumAttr(name = "Weight", attr = "weight")
      )
    )

    def vu(xs: Tuple2[String,String]*): ValUnit =
      ValUnit(
        date = LocalDate.now(),
        v = 0,
        attrs = xs.toMap
      )

    val r = rg.consume(List(
      vu("name" -> "Apple", "price" -> "23", "weight" -> "4"),
      vu("name" -> "Orange", "price" -> "10", "weight" -> "8")
    )).generate

    val pad = PadRect()

    val total =
      CellRect.ofStr("Total", Bold) + r.vTail().hTail().vTotal()

    gen("chart-total", r / pad / total)
  }

  @Test
  def testFormula() = {
    def caption(s: String) = CellRect.ofStr(s, Bold)
    val p = PadRect()

    val (h, w) = (3, 3)
    val m1 = caption("A") / mat(h, w, Some("A"))
    val m2 = caption("B") / mat(h, w, Some("B"))

    val m3 = {
      val m = SimpleRect(Cells.of(
        (1 to h).map(_ => {
          (1 to w).map(_ => {
            RectCell(typ = FormulaCell, str = "=#{Self@A}+#{Self@B}")
          })
        })))
      caption("A + B") / m
    }

    val r = m1 / p / m2 / p / m3
    gen("fml", r)
  }

  @Test
  def testMatrix() = {
    val r = {
      val sep = PadRect()
      vSandwich(sep, (1 to 5).map(p1 => {
        hSandwich(sep, (1 to 3).map(p2 => {
          val caption = CellRect.ofStr(s"$p1-$p2", Bold)
          val hHdr = HBarRect.ofStr("A", "B")
          val vHdr = VBarRect.ofStr("C", "D")
          val m = mat(2, 2)
          (caption + hHdr) / (vHdr + m)
        }):_*)
      }):_*)
    }
    gen("mats", r)
  }

  @Test
  def testBars() = {
    val r1 = VBarRect.ofStr(('a' to 'c').map(_.toString):_*)
    val r2 = HBarRect.ofStr(('1' to '5').map(_.toString):_*)
    val r = r1 + r2
    gen("bars", r)
  }

  @Test
  def testOneCell() = {
    val r = CellRect.ofStr("hello")
    gen("one-cell", r)
  }
}
