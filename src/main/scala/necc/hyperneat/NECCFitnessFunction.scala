package necc.hyperneat

import java.util
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import com.anji.integration.{Transcriber, Activator}
import com.ojcoleman.ahni.evaluation.BulkFitnessFunctionMT
import com.ojcoleman.ahni.hyperneat.Properties
import com.ojcoleman.ahni.nn.BainNN
import necc.NECCRunner.NamedDoubleAllele
import necc.ann.ActivatorContext
import necc.controller.Controller
import necc.experiment.Task
import necc.experiment.Task.{Complete, Construction, Gathering}
import necc.hyperneat.NECCFitnessFunction._
import necc.task._
import necc.Morphology
import org.jgapcustomised.{Chromosome, ChromosomeMaterial}

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.Duration

import collection.JavaConverters._

import necc.ChromosomeOps

object NECCFitnessFunction {
  case class Evaluation(genotype: ChromosomeMaterial, ann: BainNN, fitness: Double)
  case object Best
  case object Reset
  class EvaluationCollector extends Actor {
    var bestAnns = Set.empty[Evaluation]
    var bestFitness = 0.0
    override def receive: Receive = {
      case eval @ Evaluation(genotype, ann, fitness) if fitness > 0 =>
        //println(s"received eval: fitness=$fitness")
        if(fitness >= bestFitness) {
          if (fitness > bestFitness) {
            bestFitness = fitness
            bestAnns = Set(eval)
          } else bestAnns += eval
        }
      case Best => sender ! bestAnns
      case Reset =>
        bestAnns = Set.empty
        bestFitness = 0.0
    }
  }

  val defaultRep = 3
}


class NECCFitnessFunction(val task: Task) extends BulkFitnessFunctionMT {
  var rep = defaultRep
  var settings = TaskSettings.defaults

  override def init(props: Properties): Unit = {
    rep = props.getIntProperty("evaluate.repetitions", defaultRep)
    settings = TaskSettings.fromProps(props)

    super.init(props)
  }

  override def evaluate(genotypes: util.List[Chromosome]): Unit = {
    if (transcriber != null && lastBestChrom != null && props.getBooleanProperty(Morphology.TopologicalConnectivityKey)) {
      // After evaluating all genotypes, test best performing on all morphologies and apply best
      // morphology to whole population. todo: One per species instead of population?

      println(s"getting best sensor count for ${genotypes.size()} chromosome(s)")

      val (morphologyResults, sum) = findBestMorphology(transcriber, lastBestChrom)

      for {
        g <- genotypes.asScala
        d <- g.sensorCount
      } d.value = pickWeighted(morphologyResults, sum).toDouble
    }

    super.evaluate(genotypes)
  }

  @tailrec private def pickWeighted[A](from: IndexedSeq[(A, Double)], sum: Double, at: Int = 0): A = {
    from.head match {
      case (x, p) =>
        if (at < from.length - 1) x
        else if (p / sum < random.nextDouble()) x
        else pickWeighted(from, sum - p, at + 1)
    }
  }

  def findBestMorphology(transcriber: Transcriber[_ <: Activator], from: Chromosome): (IndexedSeq[(Int, Double)], Double) = {
    val morphologies = for (s <- 3 to 16) yield s
    val chromosomes = for (m <- morphologies) yield {
      val chromosome = new Chromosome(from.cloneMaterial(), 0L, 0, 0)
      for (d <- chromosome.sensorCount) d.value = m
      chromosome
    }

    import ExecutionContext.Implicits.global
    val future = Future.traverse(chromosomes)(c => Future {
      val substrate = transcriber.transcribe(c)
      try evaluate(c, substrate, -1) finally substrate.dispose()
    })
    val result = Await.result(future, Duration.Inf)
//    for ((s, f) <- morphologies zip result) println(s"chromosome with $s sensors: fitness = $f")
    (morphologies zip result, result.sum)
  }

  override def initialiseEvaluation(): Unit = {}

  var e = 0

  override def evaluate(genotype: Chromosome, substrate: Activator, evalThreadIndex: Int): Double = {
    substrate match {
      case ann: BainNN =>
        //assert(genotype.sensorCount.exists(d => math.round(d.value).toInt == ann.getInputCount))
        val sensorRange = genotype.sensorRange.fold(50.0)(_.value)
        val annContext = new ActivatorContext(ann, sensorRange)
        val controller = task match {
          case Gathering => new Controller.Gathering(annContext)
          case Construction => new Controller.Construction(annContext)
          case Complete => new Controller.Complete(annContext, annContext)
        }
        val blockLayout = BlockLayout.generate(settings.blockTypes, n = 20, min = 18f, max = 34f)
        var i = 0
        var fitness: Double = 0.0
        while (i < rep) {
          if (i > 0) annContext.reset()
          val runFitness = task.evaluate(settings, controller, blockLayout)
          fitness += runFitness
//          println(s"evaluation #${genotype.getId}-$i: runFitness = $runFitness, fitness = $fitness")
          i += 1
        }
        fitness = (fitness / rep.toDouble) max 0.0 min 1.0
        genotype.setPerformanceValue(fitness)
        //todo: penalize fitness (not performance) by time/distance/...

        e += 1

//        for (ec <- evalCollector) ec ! Evaluation(genotype.cloneMaterial(), ann, fitness)

        fitness
      case _ =>
        println("no BainNN")
        0
    }
  }
}


