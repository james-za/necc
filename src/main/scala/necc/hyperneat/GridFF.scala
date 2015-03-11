package necc.hyperneat

import com.ojcoleman.ahni.util.Point
import necc.controller.Grid
import GridFF._

trait GridFF extends NECCFitnessFunction {
  override def getNeuronPositions(layer: Int, totalLayerCount: Int): Array[Point] = {
    //println("getting neuron positions")
    layer match {
      case 0 => layerInput
      case 1 => layerHidden
      case 2 => layerOutput
    }
  }

  override def getLayerDimensions(layer: Int, totalLayerCount: Int): Array[Int] = {
    //println("getting layer dimensions")
    Array(Grid.n, Grid.n)
  }
}

object GridFF {
  def layer(z: Double) = Array.tabulate(Grid.n, Grid.n)((y, x) => {
    new Point(x.toDouble / Grid.n, y.toDouble / Grid.n, z)
  }).flatten

  val Seq(layerInput, layerHidden, layerOutput) = Seq(0.0, 0.5, 1.0) map layer
}