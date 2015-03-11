package necc.controller

import necc.JBox2DUtil._
import necc.ann.ANNContext

object Grid {
  val radius: Int = 5
  val n = 2 * radius
  val res = 5.0f
  val h = res / 2.0f
  val extent = radius * res
}

trait Grid {
  import Grid._

  def rasterize(source: Vec2, targets: Seq[Vec2]): Array[Array[Double]] = {
    val grid = Array.fill(n, n)(0.0)
    for(t <- targets) {
      val dx = t.x - source.x
      val dy = t.y - source.y
      if(-extent < dx && dx < extent && -extent < dy && dy < extent) {
        val ix = ((dx + extent) / res).toInt
        val iy = ((dy + extent) / res).toInt
        if(0 <= ix && ix < n && 0 <= iy && iy < n) {
          grid(iy)(ix) = 1.0
        }
      }
    }
    grid
  }

  def targetOffset(grid: Array[Array[Double]]): Vec2 = {
    val (max, index) = grid.flatten.zipWithIndex.maxBy(_._1)
    if(max <= Float.MinPositiveValue) Vec2(0, 0) else {
      val ox = index % n - radius
      val oy = index / n - radius
      Vec2(ox * res, oy * res)
    }
  }

  def runGrid[S : Pos, T : Pos](annCtx: ANNContext, sources: Seq[S], targets: Seq[T])
                               (action: (S, Vec2) => Unit): Unit = {
    for(source <- sources) {
      val targetPositions = targets map implicitly[Pos[T]].position
      val inputs = rasterize(implicitly[Pos[S]].position(source), targetPositions)
      val outputs = annCtx(inputs)
      val offset = targetOffset(outputs)
      action(source, offset)
    }
  }
}