package rect.Yx

case class Yx(y: Int, x: Int) {
  if (x < 0 || y < 0) throw new IllegalArgumentException(
    s"Positive y and x are expected but got x=$x and y=$y")

  def isWithin(that: Yx) = x < that.x && y < that.y

  def asRect: List[List[Yx]] =
    Range(0, y).map(y2 => Range(0, x).map(x2 => Yx(y2, x2)).toList).toList

  def +(that: Yx): Yx = Yx(y + that.y, x + that.x)
  def -(that: Yx): Yx = Yx(y - that.y, x - that.x)

  def transpose: Yx = Yx(x, y)
}

object Yx {
  val zero = Yx(0, 0)
}
