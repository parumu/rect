package rect.block

object BlockUtil {
  def vSandwich(sep: Rect, rects: Rect*): Rect = rects.reduce(_ / sep / _)
  def hSandwich(sep: Rect, rects: Rect*): Rect = rects.reduce(_ + sep + _)
}
