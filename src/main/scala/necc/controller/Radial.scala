package necc.controller

import necc.JBox2DUtil._
import necc.ann.ANNContext
import org.apache.commons.math3.util.FastMath
import org.jbox2d.common.MathUtils

object Radial {
  val turn = 2.0 * math.Pi
  def norm(radians: Double): Double = {
    var result = radians
    while (result < 0) result += turn
    while (result >= turn) result -= turn
    result
  }
  def sweep(source: Vec2, facing: Float, targets: Seq[Vec2], sectors: Int, range: Double): Array[Double] = {
    val distSq = range * range

    val result = Array.fill[Double](sectors)(0.0)

    for (target <- targets) {
      val offset = target - source
      val angle = FastMath.atan2(offset.y, offset.x)
      //val angle = math.atan2(offset.y, offset.x)
      val relativeAngle = norm(angle - facing)
      val index = (relativeAngle / turn * sectors).toInt
      val a = math.max(result(index), 1.0 - offset.length() / math.sqrt(distSq))
      result(index) = a * a * a
    }

    result
  }

  def runRadial[S : Pos : Dir, T : Pos](annCtx: ANNContext, sources: Seq[S], targets: Seq[T])
                                 (action: (S, Double, Double) => Unit): Unit = {
    val targetPositions = targets map implicitly[Pos[T]].position
    for (source <- sources) {
      val sourcePosition = implicitly[Pos[S]].position(source)
      val sourceFacing = implicitly[Dir[S]].facing(source)
      val inputs = sweep(sourcePosition, sourceFacing, targetPositions, annCtx.inputCount, annCtx.sensorRange)
      val outputs = annCtx(inputs)
      action(source, outputs(0), outputs(1))
    }
  }
}