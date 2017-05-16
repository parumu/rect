package path

import java.io.{ByteArrayInputStream, File, FileInputStream, InputStream}
import java.nio.charset.StandardCharsets

import org.apache.commons.lang3.SystemUtils

import scala.io.Source

trait Resource {
  val loc: String
  val normLoc: String
  def exists: Boolean
  def asStream: InputStream
  def asStr: String
}

abstract class FileSysRsc extends Resource {
  val onWindows = SystemUtils.IS_OS_WINDOWS
  def exists: Boolean = f.exists
  def asStream: InputStream = {
    if (f.isDirectory)
      throw new IllegalArgumentException(s"Tried to get steam from directory: ${f.getAbsolutePath}")
    new FileInputStream(normLoc)
  }
  def asStr: String = {
    if (f.isDirectory)
      throw new IllegalArgumentException(s"Tried to read from directory: ${f.getAbsolutePath}")
    Source.fromFile(normLoc).mkString("\n")
  }
  val f: File
  def genFile(fileName: String): File // works only when f is directory
  override def toString: String = normLoc
}

case class WinRes(loc: String) extends FileSysRsc {
  def getLinuxPath = {
    val s = loc.replace('\\', '/')
    if (s.startsWith("//")) s.drop(1)
    else throw new IllegalArgumentException(s"Unable to convert to linux path from: $loc")
  }
  val normLoc = if (onWindows) loc else getLinuxPath
  val f = new File(normLoc)
  def genFile(fileName: String) = new File(normLoc, fileName)
}

case class LinuxRes(loc: String) extends FileSysRsc {
  val winLoc = "s/$loc"
  val normLoc = if (onWindows) winLoc else loc
  val f = new File(normLoc)
  def genFile(fileName: String) = new File(normLoc, fileName)
}

trait UrlResState
case class Exists(s: String) extends UrlResState
case class Missing(s: String) extends UrlResState

case class UrlRes(loc: String, id: String) extends Resource {
  val normLoc = loc
  val body =
    try { Exists(Source.fromURL(loc).mkString) }
    catch {
      case ex: Throwable => Missing(ex.toString)
    }

  def exists: Boolean = body match {
    case Exists(_) => true
    case Missing(_) => false
  }

  def asStream: InputStream = body match {
    case Exists(s) => new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8))
    case Missing(s) => throw new IllegalStateException(s"Failed to fetch $loc: $s")
  }

  def asStr: String = body match {
    case Exists(s) => s
    case Missing(s) => throw new IllegalStateException(s"Failed to fetch $loc: $s")
  }
}





