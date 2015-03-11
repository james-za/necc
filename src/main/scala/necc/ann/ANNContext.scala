package necc.ann

import org.jbox2d.dynamics.World
import necc.simulation.{Structure, Block, Agent}

trait ANNContext {
  def reset(): Unit
  def apply(inputs: Array[Double]): Array[Double]
  def apply(inputs: Array[Array[Double]]): Array[Array[Double]]
}