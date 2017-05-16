package rect.renderer

import java.io.File

import rect.block.Rect
import util.FileUtil

case class TextRectRenderer(
  dir: File,
  sep: String = ","
) extends RectRenderer {

  def run(rects: Rect*): Unit = {
    rects.foreach(rect => {
      val file = new File(dir, rect.tags("filename"))
      FileUtil.withPrintWriter(FileUtil.prepareToWrite(file), pw => {
        rect.as2DimSeq.foreach(rowCells => {
          pw.print(rowCells.map(_.toString).mkString(sep) + "\n")
        })
      })})
  }
}
