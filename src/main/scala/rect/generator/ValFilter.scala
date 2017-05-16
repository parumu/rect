package rect.generator

trait ValFilter {
  def apply(vu: ValUnit): ValUnit
}

case object AsIs extends ValFilter {
  def apply(vu: ValUnit): ValUnit = vu
}