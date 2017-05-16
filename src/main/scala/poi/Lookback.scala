package poi

abstract sealed class Lookback

case object UnlimitedLookback extends Lookback

case class LookbackRow(n: Int) extends Lookback {
  if (n < 1) throw new IllegalArgumentException(
    s"Lookback rows must be positive, but got: $n"
  )
}