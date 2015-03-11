package necc.ann

import org.jbox2d.dynamics.World
import com.ojcoleman.ahni.nn.BainNN
import necc.JBox2DUtil._
import org.jbox2d.common.Mat22
import necc.simulation.{Structure, Block, Category, Agent}

class BainNNContext(ann: BainNN) extends ANNContext {
  override def reset(): Unit = ann.reset()
  override def apply(inputs: Array[Double]): Array[Double] = ann.next(inputs)
  override def apply(inputs: Array[Array[Double]]): Array[Array[Double]] = ann.next(inputs)
}
