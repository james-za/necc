package necc.ann

import org.jbox2d.dynamics.World
import necc.simulation.{Block, Structure, Agent}

class TestANNContext extends ANNContext {
  /** v ∈ [0, 3) */
  def linearVelocity: Double = math.random * 3

  /** ω ∈ (-1.5, -0.5] ∪ [0.5, 1.5) */
  def angularVelocity: Double = {
    val a = math.random - 0.5
    if (a < 0) (-1.5) - a else a + 1
  }

  override def reset(): Unit = {}

  override def apply(inputs: Array[Double]): Array[Double] =
    Array(linearVelocity, angularVelocity)

  override def apply(inputs: Array[Array[Double]]): Array[Array[Double]] =
    Array(Array(linearVelocity), Array(angularVelocity))
}
