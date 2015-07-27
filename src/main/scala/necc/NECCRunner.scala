package necc

import java.io.FileWriter
import java.text.DecimalFormat
import java.util
import java.util.{Iterator, Random}
import java.util.concurrent.TimeUnit

import akka.actor.Props
import akka.util.Timeout
import com.anji.integration.{XmlPersistableAllele, XmlPersistableChromosome}
import com.anji.neat.{ConnectionAllele, NeuronAllele, NEATGenotype}
import com.anji.persistence.FilePersistence
import com.github.tototoshi.csv
import com.ojcoleman.ahni.event.{AHNIEvent, AHNIEventListener}
import com.ojcoleman.ahni.hyperneat.{HyperNEATConfiguration, HyperNEATEvolver, Properties}
import com.ojcoleman.ahni.integration.{ParamCollection, ParamGene, ParamAllele}
import com.ojcoleman.ahni.nn.BainNN
import com.ojcoleman.ahni.transcriber.HyperNEATTranscriberBain
import necc.gui.NECCGUI
import necc.hyperneat.NECCFitnessFunction.EvaluationCollector
import necc.hyperneat.RadialFF
import org.jgapcustomised.impl.IntegerAllele
import org.jgapcustomised._
import resource.Using

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.reflect.ClassTag
import scalafx.application.Platform

object NECCRunner {
  val nf = new DecimalFormat("0.0000")
  
  trait NamedAllele { self: Allele =>
    def name: String
  }
  
  class MorphologyMutator(rate: Double) extends MutationOperator(rate) {
    override def mutate(config: Configuration, target: ChromosomeMaterial, allelesToAdd: util.Set[Allele], allelesToRemove: util.Set[Allele]): Unit = {
      target.getAlleles.asScala foreach {
        case a: NamedDoubleAllele =>
          a.setToRandomValue(config.getRandomGenerator, onlyPerturbFromCurrentValue = true)
        case _ =>
      }
    }
  }

  class NamedDoubleAllele(iid: Long, var value: Double, val name: String, val min: Double, val max: Double)
    extends Allele(new Gene(iid)) {
    val StdDev = 1.0
    val beforeMax = java.lang.Math.nextAfter(max, min)
    val isVariable = min < max
    setValue(value)
    def this(config: HyperNEATConfiguration, value: Double, name: String, min: Double, max: Double) =
      this(config.nextInnovationId(), value, name, min, max)

    def isEquivalent(otherAllele: Allele): Boolean = otherAllele match {
      case other: NamedDoubleAllele => value == other.value
      case _ => false
    }

    override def cloneAllele(): Allele = new NamedDoubleAllele(getGene.getInnovationId, value, name, min, max)

    override def getValue: Double = value
    override def setValue(aValue: Double): Unit = value = aValue

    override def setToRandomValue(a_numberGenerator: Random, onlyPerturbFromCurrentValue: Boolean): Unit = {
      if (isVariable) {
        if (onlyPerturbFromCurrentValue) {
          value += a_numberGenerator.nextGaussian * StdDev
          if (value < min) value = min
          if (value >= max) value = beforeMax
        } else {
          value = min + a_numberGenerator.nextDouble() * (max - min)
        }
      }
    }
  }

  def runEvolver[T](properties: Properties, run: Int, tries: Int)(f: (HyperNEATEvolver, Properties) => T): Option[T] = {
    var i = 0
    var result: Option[T] = None
    while (i < tries && result.isEmpty) {
      val runProps = new Properties(properties)
      runProps.setProperty("run.id", s"${run * tries + i}")
      val evolver = runProps.singletonObjectProperty(classOf[HyperNEATEvolver])
      val gens = runProps.getDoubleProperty("num.generations")
      evolver.addEventListener(new AHNIEventListener {
        var gen = 0
        override def ahniEventOccurred(e: AHNIEvent): Unit = {
          if (e.getType == AHNIEvent.Type.GENERATION_END) {
            gen += 1
            val genProgress = gen.toDouble / gens
            Platform.runLater {
              NECCGUI.progress.progress = genProgress
            }
          }
        }
      })
      result = try {
        val mat = evolver.getConfig.getSampleChromosomeMaterial
        val initialSensorCount = properties.getIntProperty("evaluate.sensors.count")
        val sensorCountAllele = new NamedDoubleAllele(evolver.getConfig, value = initialSensorCount, "sensor.count", 3, 16)
        val sensorRangeAllele = new NamedDoubleAllele(evolver.getConfig, value = 50.0, "sensor.range", 50.0, 50.0)
        mat.getAlleles.add(sensorCountAllele)
        mat.getAlleles.add(sensorRangeAllele)

        val useAdaptiveMorphology = runProps.getBooleanProperty(Morphology.AdaptiveKey)
        if (useAdaptiveMorphology) {
          val rate = runProps.getDoubleProperty(Morphology.AdaptiveRateKey, 1.0)
          evolver.getConfig.addMutationOperator(new MorphologyMutator(rate))
        }

        val initialGenotype = NEATGenotype.randomInitialGenotype(runProps, evolver.getConfig)

        val useTopologicalConnectivity = runProps.getBooleanProperty(Morphology.TopologicalConnectivityKey)
        if (useAdaptiveMorphology || useTopologicalConnectivity) {
          for (c <- initialGenotype.getChromosomes.asScala) {
            c.getAlleles.asScala foreach {
              case d: NamedDoubleAllele =>
                d.setToRandomValue(evolver.getConfig.getRandomGenerator, onlyPerturbFromCurrentValue = false)
                println(s"Chromosome #${c.getId}: ${d.name} = ${d.getValue}")
              case _ =>
            }
          }
        }

        evolver.run(initialGenotype)
        Some(f(evolver, runProps))
      } catch {
        case ex: Exception => ex.printStackTrace(); None
      } finally {
        evolver.dispose()
      }
      i += 1
    }
    result
  }

  def run(properties: Properties, material: Option[ChromosomeMaterial] = None) = {

    if (!properties.containsKey("run.id")) {
      properties.setProperty("run.id", "0")
    }
    val initialSensorCount = properties.getIntProperty("evaluate.sensors.count")
    changeSensorCount(properties, initialSensorCount)
    val numRuns = properties.getIntProperty(HyperNEATConfiguration.NUM_RUNS_KEY)

    var run = 0
    while (run < numRuns) {
      val result = runEvolver(properties, run, tries = 20) { (evolver, runProps) =>
        val performances = evolver.getBestPerformance
        val bestOpt = try {
          val chromosomes = evolver.getBesttPerformingChromosomesForEachGen
          val results = chromosomes zip performances
          if (results.nonEmpty) Some(results.maxBy(_._2)) else None
        } catch {
          case ex: Exception =>
            ex.printStackTrace()
            for {
              c <- Option(evolver.getBestPerformingFromLastGen)
              f <- Option(c.getPerformanceValue)
            } yield (c, f)
        }
        for (best <- bestOpt) {
          val fileXml = NECCGUI.outputPath(best._2, run, ".xml").toFile
          for (out <- Using.fileWriter()(fileXml)) {
            out.write(toXml(best._1))
          }
          val fileCsv = NECCGUI.outputPath(best._2, run, ".csv").toFile
          for (out <- Using.fileWriter()(fileCsv)) {
            val w = csv.CSVWriter.open(out)
            w.writeRow(Seq("generation", "fitness"))
            for ((f, g) <- performances.zipWithIndex) w.writeRow(Seq(g.toString, f.toString))
            w.close()
          }
          val fileProps = NECCGUI.outputPath(best._2, run, ".properties").toFile
          for (out <- Using.fileOutputStream(fileProps)) {
            runProps.storeToXML(out, fileProps.getName)
          }
        }
        bestOpt.fold(0.0)(_._2)
      }
      result.getOrElse(0.0)

      val runProgress = (run + 1.0) / numRuns
      Platform.runLater {
        NECCGUI.progress.progress = 0
        NECCGUI.progress2.progress = runProgress
      }

      run += 1
    }
  }

  def toXml(chromosome: Chromosome): String = {
    val result = new StringBuffer
    result.append(s"""<chromosome id="${chromosome.getId}"""")
    if (chromosome.getPrimaryParentId != null) {
      result.append(s""" primary-parent-id="${chromosome.getPrimaryParentId}"""")
    }
    if (chromosome.getSecondaryParentId != null) {
      result.append(s""" secondary-parent-id="${chromosome.getSecondaryParentId}"""")
    }
    result.append(">\n")
    val iter = chromosome.getAlleles.iterator
    while (iter.hasNext) {
      val allele = iter.next
      result.append(toXml(allele))
    }
    result.append(s"</chromosome>")
    result.toString
  }

  def toXml(allele: Allele): String = {
    val result: StringBuffer = new StringBuffer
    allele match {
      case nAllele: NeuronAllele =>
        result.append(s"""<neuron id="${allele.getInnovationId}" type="${nAllele.getType.toString}" """)
        result.append(s"""activation="${nAllele.getActivationType}"/>""")
      case cAllele: ConnectionAllele =>
        result.append(s"""<connection id="${allele.getInnovationId}" src-id="${cAllele.getSrcNeuronId}" """)
        result.append(s"""dest-id="${cAllele.getDestNeuronId}" weight="${cAllele.getWeight}"/>""")
      case d: NamedDoubleAllele =>
        result.append(s"""<param id="${allele.getInnovationId}" """)
        result.append(s"""name="${d.name}" value="${d.value}" min="${d.min}" max="${d.max}"/>""")
    }
    result.append("\n")
    result.toString
  }
}