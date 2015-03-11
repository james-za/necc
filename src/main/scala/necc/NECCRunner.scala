package necc

import java.io.File
import java.text.DecimalFormat
import java.util.concurrent.TimeUnit

import akka.actor.Props
import akka.util.Timeout
import com.anji.neat.NEATGenotype
import com.ojcoleman.ahni.event.{AHNIEvent, AHNIEventListener}
import com.ojcoleman.ahni.hyperneat.{HyperNEATConfiguration, HyperNEATEvolver, Properties}
import necc.gui.{RunData, Run, Gen, NECCGUI}
import necc.hyperneat.NECCFitnessFunction.{Evaluation, Best, EvaluationCollector}
import necc.task.TaskSettings
import org.jgapcustomised.{Chromosome, ChromosomeMaterial}
import sbinary.Operations

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scalafx.application.Platform

object NECCRunner {
  val nf = new DecimalFormat("0.0000")

  def run(properties: Properties, material: Option[ChromosomeMaterial] = None) = {

    @tailrec def runEvolver(evolver: HyperNEATEvolver, tries: Int): Array[Double] = {
      if (tries < 1) Array.empty else try {
        evolver.init(new Properties(properties))
        val performance = material.fold(evolver.run()) { m =>
          val conf = evolver.getConfig
          val size = conf.getPopulationSize
          val chromosomes = new java.util.ArrayList[Chromosome](size)
          for (_ <- 0 until size) {
            val newMaterial = m.clone(null)
            for (operator <- evolver.getConfig.getMutationOperators.asScala) {
              operator.mutate(evolver.getConfig, newMaterial)
            }
            chromosomes.add(new Chromosome(newMaterial, conf.nextChromosomeId(), conf.getObjectiveCount, conf.getNoveltyObjectiveCount))
          }
          val genotype = new NEATGenotype(properties, conf, chromosomes)

          evolver.run(genotype)
        }
        evolver.dispose()
        performance
      } catch {
        case ex: IndexOutOfBoundsException => runEvolver(evolver, tries - 1)
      }
    }

    if (!properties.containsKey("run.id")) {
      properties.setProperty("run.id", "0")
    }

    val numRuns = properties.getIntProperty(HyperNEATConfiguration.NUM_RUNS_KEY)

    val evolver = new HyperNEATEvolver

    var gen = 0
    evolver.addEventListener(new AHNIEventListener {
      val gens = properties.getDoubleProperty("num.generations")
      override def ahniEventOccurred(e: AHNIEvent): Unit = {
        if (e.getType == AHNIEvent.Type.GENERATION_END) {
          gen += 1
          Platform.runLater {
            NECCGUI.progress.progress = gen.toDouble / gens
          }
        }
      }
    })

    val task = NECCGUI.taskCombo.getSelectionModel.getSelectedItem

    import akka.pattern.ask
    import concurrent.ExecutionContext.Implicits.global
    implicit val timeout = Timeout(30, TimeUnit.SECONDS)

    val paths = for {
      run <- 0 until numRuns
    } yield {
      val collector = necc.actorSystem.actorOf(Props[EvaluationCollector])
      properties.setProperty("collector.path", collector.path.toString)
      val performance = runEvolver(evolver, tries = 50)

      (collector ? Best).mapTo[Set[Evaluation]].onSuccess {
        case bestEvals => for (eval <- bestEvals.headOption) {
          val gens = performance.zipWithIndex.map {
            case (fitness, g) => Gen(g, fitness)
          }
          val runs = Array(Run(run, gens))
          val runData = RunData(task, bestEvals.map(e => (e.genotype, e.ann)), runs)

          val runDataPath = NECCGUI.outputPath(eval.fitness, run)

          import necc.gui.RunDataProtocol._
          Operations.toFile(runData)(runDataPath.toFile)
          actorSystem.stop(collector)
          NECCGUI.loadRuns(Some(runDataPath))
          gen = 0
          //Platform.runLater {
            NECCGUI.progress.progress = 0
            NECCGUI.progress2.progress = (run + 1.0) / numRuns
          //}
        }
      }
    }

    //Platform.runLater {
      NECCGUI.startRun.disable = false
      NECCGUI.resumeRun.disable = false
      NECCGUI.taskCombo.disable = false
      NECCGUI.progress.disable = true
      NECCGUI.progress2.disable = true
    //}
  }
}
