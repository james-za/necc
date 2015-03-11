package necc.controller

import necc.JBox2DUtil._
import necc.ann.ANNContext
import org.jbox2d.common.MathUtils

object Radial {
  val sectors = 8
  val sectorSize = math.Pi / sectors
  val distSq = 50 * 50
}

trait Radial {
  import Radial._

  def sweep(source: Vec2, targets: Seq[Vec2]): Vector[Double] = {
    var i = 0
    val bsb = Vector.newBuilder[Double]
    while(i < sectors) {
      val from = i * sectorSize
      val to = (i + 1) * sectorSize
      bsb += (if(targets.exists { target =>
        val dir = target - source
        if (dir.lengthSquared() <= distSq) {
          val angle = MathUtils.atan2(dir.y, dir.x)
          val angleNorm = if(angle < 0) angle + math.Pi / 2 else angle
          from <= angleNorm && angleNorm < to
        } else false
      }) 1.0 else 0.0)
      i += 1
    }
    bsb.result()
  }

  def runRadial[S : Pos, T : Pos](annCtx: ANNContext, sources: Seq[S], targets: Seq[T])
                                 (action: (S, Double, Double) => Unit): Unit = {
    for(source <- sources) {
      val targetPositions = targets map implicitly[Pos[T]].position
      val ts = sweep(implicitly[Pos[S]].position(source), targetPositions)
      val inputs = Array(
        ts(0), ts(7),
        ts(1), ts(6),
        ts(2), ts(5),
        ts(3), ts(4))
      val outputs = annCtx(inputs)
      action(source, outputs(0), outputs(1))
    }
  }
}