package util

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate, ZoneId}
import java.util.Date

object DateUtil {
  def getPrevBd(d: LocalDate) =
  d.getDayOfWeek match {
    case DayOfWeek.MONDAY => d.plusDays(-3)
    case DayOfWeek.SUNDAY => d.plusDays(-2)
    case _ => d.plusDays(-1)
  }

  def getNextBd(d: LocalDate) =
    d.getDayOfWeek match {
      case DayOfWeek.FRIDAY => d.plusDays(3)
      case DayOfWeek.SATURDAY => d.plusDays(2)
      case _ => d.plusDays(1)
    }

  def isWeekday(date: LocalDate): Boolean =
    Seq(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY).contains(date.getDayOfWeek)

  // TODO implement this
  def isHoliday(date: LocalDate, holCodes: Seq[String]) = ???

  def isXBdAfterHolidays(date: LocalDate, holCodes: Seq[String], x: Int): Boolean = {
    def isHol(date: LocalDate): Boolean = isHoliday(date, holCodes)

    // check if x weekdays in the past from today are all non-holidays
    val (areNonLastDaysWeekdays, d) = (1 to x).foldLeft((true, date))((acc, _) => {
      val (b, d) = acc
      val pd = getPrevBd(d)
      if (!isWeekday(d) || isHol(d)) (false, pd)
      else (b, pd)
    })
    // x weekday ago should a weekday holiday
    val isLastDayHoliday = isWeekday(d) && isHol(d)

    areNonLastDaysWeekdays && isLastDayHoliday
  }

  def isBetween(beg: LocalDate, end: LocalDate, d: LocalDate) = {
    (beg.isBefore(d) || beg.isEqual(d)) && (end.isAfter(d) || end.isEqual(d))
  }

  implicit def java8LocalDate2JavaUtilDate(x: java.time.LocalDate): java.util.Date =
    Date.from(x.atStartOfDay(ZoneId.systemDefault()).toInstant)

  implicit def java8LocalDate2jodaLocalDate(x: java.time.LocalDate): org.joda.time.LocalDate =
    new org.joda.time.LocalDate(x.getYear, x.getMonthValue, x.getDayOfMonth)

  object formatters {
    val yyyy_MM_dd = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val yyyyMMdd = DateTimeFormatter.ofPattern("yyyyMMdd")
  }
}
