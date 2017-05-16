package rect.renderer

import rect.block.Rect

trait RectRenderer {
  def run(rects: Rect*): Unit
}
