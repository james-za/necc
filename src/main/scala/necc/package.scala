import akka.actor.ActorSystem
import com.ojcoleman.ahni.hyperneat.Properties
import com.ojcoleman.ahni.transcriber.HyperNEATTranscriber
import necc.NECCRunner.NamedDoubleAllele
import necc.hyperneat.RadialFF
import org.jgapcustomised.Chromosome
import collection.JavaConverters._

package object necc {
  val actorSystem = ActorSystem()

  object Morphology {
    val TopologicalConnectivityKey = "morphology.topological.connectivity"
    val AdaptiveKey = "morphology.adaptive"
    val AdaptiveRateKey = "morphology.adaptive.rate"
  }

  implicit class ChromosomeOps(val chromosome: Chromosome) extends AnyVal {
    def getNamedAllele(name: String): Option[NamedDoubleAllele] = chromosome.getAlleles.asScala.collectFirst {
      case d: NamedDoubleAllele if d.name == name => d
    }
    def sensorCount = getNamedAllele("sensor.count")
    def sensorRange = getNamedAllele("sensor.range")
    def sensorFOV = getNamedAllele("sensor.fov")
    def sensorCoverage = getNamedAllele("sensor.coverage")
  }

  def changeSensorCount(properties: Properties, sensorCount: Int): Unit = {
    properties.setProperty(HyperNEATTranscriber.SUBSTRATE_WIDTH, s"1,1,1")
    properties.setProperty(HyperNEATTranscriber.SUBSTRATE_HEIGHT, s"$sensorCount,$sensorCount,2")
    for (layer <- 0 until 3) {
      val neuronPositions = RadialFF.neuronPositions(sensorCount, layer).mkString(",")
      val layerKey = HyperNEATTranscriber.NEURON_POSITIONS_FOR_LAYER + layer
      properties.setProperty(layerKey, neuronPositions)
    }
  }
}
