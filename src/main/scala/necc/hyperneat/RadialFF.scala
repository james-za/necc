package necc.hyperneat

import com.ojcoleman.ahni.util.Point
import RadialFF._

trait RadialFF extends NECCFitnessFunction {
  override def getNeuronPositions(layer: Int, totalLayerCount: Int): Array[Point] = {
    //println("getting neuron positions")
    layer match {
      case 0 => layerInput(settings.sensors)
      case 2 => layerOutput
      case _ => layerHidden(settings.sensors)
    }
  }

  override def getLayerDimensions(layer: Int, totalLayerCount: Int): Array[Int] = {
    //println("getting layer dimensions")
    layer match {
      case 2 => Array(1, 2)
      case _ => Array(1, settings.sensors)
    }
  }
}

object RadialFF {
  val midX = 0.5
  val midY = 0.5
  val radius = 0.5
  val facing = math.Pi / 2.0

  // radial inputs
  def layerInput(n: Int) = {
    //val angles = (math.Pi / 8.0 to 2 * math.Pi by math.Pi / 4.0).toVector
    val d = 2.0 * math.Pi / n.toDouble
    val a = facing + d / 2.0
    Array.tabulate(n) { i =>
      val angle = i * d + a
      new Point(
        midX + radius * math.cos(angle),
        midY + radius * math.sin(angle),
        0.0
      )
    }
  }
  def layerHidden(n: Int) = layerInput(n).map(p => new Point(p.x, p.y, 0.5))
  def layerOutput = Array(
    new Point(midX, midY + radius * 0.6, 1.0), // linear velocity
    new Point(midX, midY, 1.0)) // angular velocity
}