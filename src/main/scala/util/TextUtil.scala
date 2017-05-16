package util

import scala.util.matching.Regex

object TextUtil {
  implicit class CaseInsensitiveCompHelper(val sc: StringContext) extends AnyVal {
    def ci: Regex = ("(?i)" + sc.parts.mkString).r
  }
}
