package rect.renderer

import java.io.{File, FileOutputStream}

import org.apache.poi.ss.usermodel._
import org.apache.poi.xssf.streaming.SXSSFWorkbook
import org.slf4j.{Logger, LoggerFactory}
import poi.CellsRows
import rect.block._
import util.FileUtil

case class ExcelRectRender(
  file: File,
  colWidth: Option[Int] = Some(7),
  zoom: Option[Int] = Some(80),
  dateFmt: String = "yyyymmdd",
  resizeCols: Boolean = true
)(implicit cellStyleBuilder: CellStyleBuilder) extends RectRenderer {
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  def run(rects: Rect*): Unit = {
    log.info("Rendering Spreadsheet...")
    val wb = new SXSSFWorkbook(-1) // need unlimited lookback for column resizing
    val cellStyles = cellStyleBuilder.build(wb)
    val resolver = FormulaExpr(rects:_*)

    try {
      rects.foreach(rect => {
        val name = rect.tags("sheet")
        log.info(s"Rendering $name...")
        val sh = createSheet(wb, name)
        writeRectToSheet(wb, sh, rect, cellStyles, resolver)
        if (resizeCols) {
          log.info("Resizing cols...")
          (0 until rect.getDim.x).foreach(sh.autoSizeColumn)
        }
      })
      writeToFile(wb, file)
    }
    finally {
      wb.dispose() // make sure to remove wb tmp file which can be huge
    }
  }

  def getTime(beg: Long): String = {
    val sec = (System.currentTimeMillis - beg) / 1000
    val min = sec / 60.0
    f"$sec%d src or $min%.2f min"
  }

  def writeToFile(wb: Workbook, file: File): Unit = {
    FileUtil.prepareToWrite(file)
    if (file.exists) file.delete
    val os = new FileOutputStream(file)
    try {
      val writeBeg = System.currentTimeMillis()
      log.info(s"Writing spreadsheet in memory to ${file.getAbsolutePath}...")
      wb.write(os)
      val wrtTime = getTime(writeBeg)
      log.info(s"Writing spreadsheet finished in $wrtTime")
    }
    finally {
      os.close()
    }
  }

  def createSheet(wb: Workbook, name: String): Sheet = {
    val sh = wb.createSheet(name)
    if (colWidth.isDefined) sh.setDefaultColumnWidth(colWidth.get)
    if (zoom.isDefined) sh.setZoom(zoom.get, 100)
    sh
  }

  // optimized for speed
  def writeRectToSheet(
    wb: Workbook,
    sh: Sheet,
    rect: Rect,
    cellStyles: Map[CellStyleKey,CellStyle],
    resolver: FormulaExpr
  ) = {
    val crs = CellsRows(sh)

    rect.decompose.foreach { case (offset, r) =>
      val dimY = r.getDim.y
      val dimX = r.getDim.x
      val offsetY = offset.y
      val offsetX = offset.x

      var ry = 0
      while(ry < dimY) {
        val ay = offsetY + ry
        val row = crs.getRow(ay)

        var rx = 0
        while(rx < dimX) {
          val ax = offsetX + rx
          val shCell = crs.getCell(row, ax)
          val c = r.getCell(ry, rx)

          if (c.typ == NumCell) {
            if (c.style != null) shCell.setCellStyle(cellStyles(c.style))
            shCell.setCellValue(c.num)
          }
          else if (c.typ == StrCell) {
            if (c.style != null) shCell.setCellStyle(cellStyles(c.style))
            shCell.setCellValue(c.str)
          }
          else if (c.typ == DateCell) {
            if (c.style != null) shCell.setCellStyle(cellStyles(c.style))
            import util.DateUtil._
            shCell.setCellValue(c.date)
          }
          else if (c.typ == FormulaCell) {
            if (c.style != null) shCell.setCellStyle(cellStyles(c.style))
            val fml = resolver.eval(c.str, ay, ax, rect)
            shCell.setCellFormula(fml)
          }
          rx += 1
        }
        ry += 1
      }
    }
  }

}

