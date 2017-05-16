package util

import java.io.{File, PrintWriter}

object FileUtil {
  def withPrintWriter(f: File, proc: PrintWriter => Unit): Unit = {
    try {
      val pw = new PrintWriter(f)
      try {
        proc(pw)
      }
      finally {
        pw.close()
      }
    } catch {
      case ex: Throwable => throw new IllegalStateException(s"Failed to write ${f.getAbsolutePath}: $ex")
    }
  }

  def prepareToWrite(f: File): File = {
    val dir = f.getParentFile
    if (!dir.exists && !dir.mkdirs)
      throw new IllegalStateException(s"Failed to create dir: ${dir.getAbsolutePath}")
    f
  }
}
