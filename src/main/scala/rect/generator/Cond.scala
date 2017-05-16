package rect.generator

case class Not(cond: Cond) extends Cond {
  def pred(inVu: Option[ValUnit]): Boolean = !cond.pred(inVu)
}

case object True extends Cond {
  def pred(inVu: Option[ValUnit]): Boolean  = true
}

case object False extends Cond {
  def pred(inVu: Option[ValUnit]): Boolean  = false
}

trait Cond {
  def pred(inVu: Option[ValUnit]): Boolean
  def apply(vu: Option[ValUnit]): Option[ValUnit] = if (pred(vu)) vu else None

}
