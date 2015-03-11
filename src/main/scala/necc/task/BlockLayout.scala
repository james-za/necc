package necc.task

import necc.JBox2DUtil.Vec2
import necc.simulation.Block
import org.jbox2d.common.MathUtils

object BlockLayout {
  def generate(blockTypes: Seq[Block.Type], n: Int, min: Float, max: Float): Vector[(Vec2, Block.Type)] = {
    val d = MathUtils.TWOPI / n
    Vector.tabulate(n) { i =>
      val btIndex = util.Random.nextInt(blockTypes.size)
      val bt = blockTypes(btIndex)
      val r = min + util.Random.nextFloat() * (max - min)
      val angle = i * d
      val x = r * MathUtils.cos(angle)
      val y = r * MathUtils.sin(angle)
      (Vec2(x, y), bt)
    }
  }
}
