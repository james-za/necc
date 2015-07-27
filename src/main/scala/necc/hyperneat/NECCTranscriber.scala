package necc.hyperneat

import java.util

import com.ojcoleman.ahni.hyperneat.Properties
import com.ojcoleman.ahni.nn.BainNN
import com.ojcoleman.ahni.transcriber.{HyperNEATTranscriber, HyperNEATTranscriberBain}
import org.jgapcustomised.Chromosome
import necc._

class NECCTranscriber extends HyperNEATTranscriberBain {
  var properties: Properties = null

  def this(props: Properties) = {
    this()
    init(props)
  }


  override def init(props: Properties): Unit = {
    properties = props
    super.init(props)
  }

  override def newBainNN(genotype: Chromosome, substrate: BainNN, options: util.Map[String, AnyRef]): BainNN = {
    NECCTranscriber.lock.synchronized {
      for (d <- genotype.sensorCount) {
        val sensors = math.round(d.getValue).toInt
//        val propsNotUpdated = properties.getProperty(HyperNEATTranscriber.SUBSTRATE_HEIGHT) != s"$sensors,$sensors,2"
//        if (sensors != getHeight()(0) || propsNotUpdated) {
          changeSensorCount(properties, sensors)
          init(properties)
//        }
      }
      // input count can fail to update if substrate is reused, so pass null to recreate it
      val ann = super.newBainNN(genotype, null, options)
      ann
    }
  }
}

object NECCTranscriber {
  val lock = new Object()
}