package necc.ann

import com.anji.integration.Activator
import org.jbox2d.dynamics.World
import com.ojcoleman.ahni.nn.BainNN
import necc.JBox2DUtil._
import org.jbox2d.common.Mat22
import necc.simulation.{Structure, Block, Category, Agent}

class ActivatorContext(ann: Activator, val sensorRange: Double) extends ANNContext {
  override def reset(): Unit = ann.reset()
  override def apply(inputs: Array[Double]): Array[Double] = ann.next(inputs)
  override def inputCount: Int = ann.getInputCount
  override def outputCount: Int = ann.getOutputCount
}
