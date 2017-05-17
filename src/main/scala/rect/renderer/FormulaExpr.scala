package rect.renderer

import org.apache.poi.ss.util.CellReference
import org.slf4j.{Logger, LoggerFactory}
import rect.Yx.Yx
import rect.block.Rect

import scala.collection.mutable.{Map => MutMap}
import scala.util.matching.Regex

/*
  Formula Exp

  Grammar: #{expr[@rect]}

  Available expr:
  Self       - address of current cell of src rect in dest rect
  SelfRow    - row of current cell of src rect in dest rect
  SelfCol    - col of current cell of src rect in dest rect
  AbsSelf    - adress of current cell of src rect in outermost rect
  SelfAbsRow - row of current cell of src rect in outermost rect
  SelfAbsCol - col of current cell of src rect in outermost rect
  (y, x)     - adress of coordinate of src rect in dest rect
  Height     - height of the dest rect
  Width      - width of the dest rect
  $x         - attr x of the rext e.g. $home

  "@rect" instructs to move to the specified rect before starting evaluation process

  ** Excep for attr and rect name, expr is case-insensitive
*/
case class FormulaExpr(rects: Rect*) {
  val tagRe = new Regex("""#\{([^}]+)\}""", "expr")
  val exprRe = """(?i)(Self|SelfCol|SelfRow|AbsSelf|AbsSelfCol|AbsSelfRow|\(\s*\d+,\s*\d+\)|Height|Width|\$\w+)(?:@(\w+))?""".r
  val coordRe = """\(\s*(\d+),\s*(\d+)\)""".r
  val attrRe = """\$(\w+)""".r

  val namedRects: Map[String,Rect] = {
    def loop(rects: List[Rect]): List[Rect] = {
      rects match {
        case Nil => List.empty
        case xs => xs.flatten(x => x :: loop(x.children))
      }
    }
    loop(rects.toList).filter(_.optName.isDefined).map(x => x.optName.get -> x).toMap
  }

  val absLocCache = MutMap.empty[(String,String),Yx]

  def getAbsLocation(root: Rect, tgt: Rect): Yx = {
    val key = (root.optName.orNull, tgt.optName.orNull)
    absLocCache.get(key) match {
      case Some(yx) => yx
      case None =>
        def loop(r: Rect): Option[Yx] = {
          if (r == tgt) Some(r.optRelLoc.getOrElse(Yx.zero)) // can be None if root element
          else {
            if (r.children.isEmpty) None
            else r.children.flatMap(x => {
              loop(x).map(_ + r.optRelLoc.getOrElse(Yx.zero))
            }).headOption
          }
        }
        val yx = loop(root) match {
          case None => throw new IllegalStateException(s"No such $tgt under $root")
          case Some(x) => x
        }
        absLocCache += (key -> yx)
        yx
    }
  }

  var selfCache = MutMap.empty[(Int,Int),String]
  var selfColCache = MutMap.empty[Int,String]
  var coordCache = MutMap.empty[(Int,Int),String]
  var absSelfColCache = MutMap.empty[Int,String]

  val log: Logger = LoggerFactory.getLogger(this.getClass)

  // optimized for speed
  def eval(formula: String, y: Int, x: Int, root: Rect): String = {
    val tagExprs = tagRe.findAllMatchIn(formula).map(m => (m.toString, m.group("expr"))).toList

    val tagInstRectNames = tagExprs.map {
      case (tag, exprRe(inst, rectName)) => (tag, inst, Option(rectName))
    }.toIndexedSeq

    var i = 0
    var numItems = tagInstRectNames.length
    var fml = formula
    while(i < numItems) {
      val (tag, inst, rectName) = tagInstRectNames(i)

      val destRect = namedRects.getOrElse(rectName.orNull, root)
      val destAbsLoc = getAbsLocation(root, destRect)
      val srcAbsLoc = getAbsLocation(root, root.getRectFor(Yx(y, x)))
      val relY = y - srcAbsLoc.y
      val relX = x - srcAbsLoc.x

      var repl: String = null

      if (inst == "Self") {
        val yy = destAbsLoc.y + relY
        val xx = destAbsLoc.x + relX
        val k = (yy, xx)
        if (selfCache.contains(k)) repl = selfCache(k)
        else {
          repl = new CellReference(yy, xx).formatAsString()
          selfCache += (k -> repl)
        }
      }
      else if (inst ==  "SelfCol") {
        val xx = destAbsLoc.x + relX
        if (selfColCache.contains(xx)) repl = selfColCache(xx)
        else {
          repl = CellReference.convertNumToColString(xx)
          selfColCache += (xx -> repl)
        }
      }
      else if (inst == "SelfRow") repl = (destAbsLoc.y + relY).toString
      else if (inst == "AbsSelf") {
        var xStr: String = null
        val xx = destAbsLoc.x + relX
        if (absSelfColCache.contains(xx)) xStr = absSelfColCache(xx)
        else {
          xStr = CellReference.convertNumToColString(xx)
          absSelfColCache += (xx -> xStr)
        }
        repl = xStr + (destAbsLoc.y + relY + 1).toString
      }
      else if (inst == "AbsSelfCol") {
        val x = destAbsLoc.x + relX
        if (absSelfColCache.contains(x)) repl = absSelfColCache(x)
        else {
          repl = CellReference.convertNumToColString(x)
          absSelfColCache += (x -> repl)
        }
      }
      else if (inst == "AbsSelfRow") repl = (destAbsLoc.y + relY + 1).toString
      else if (inst == "Height") repl = destRect.getDim.y.toString
      else if (inst == "Width") repl = destRect.getDim.x.toString
      else {
        inst match {
          case coordRe(toY, toX) =>
            val yy = destAbsLoc.y + toY.toInt
            val xx = destAbsLoc.x + toX.toInt
            val k = (yy, xx)
            if (coordCache.contains(k)) repl = coordCache(k)
            else {
              repl = new CellReference(yy, xx).formatAsString()
              coordCache += (k -> repl)
            }

          case attrRe(attr) => repl = destRect.tags.getOrElse(attr, "")
          case s => throw new IllegalStateException(s"Unexpected expr: $s")
        }
      }
      fml = fml.replaceAllLiterally(tag, repl)
      i += 1
    }
    fml
  }
}














