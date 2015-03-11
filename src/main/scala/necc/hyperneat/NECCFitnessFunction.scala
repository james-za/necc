package necc.hyperneat

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import com.anji.integration.Activator
import com.ojcoleman.ahni.evaluation.BulkFitnessFunctionMT
import com.ojcoleman.ahni.hyperneat.Properties
import com.ojcoleman.ahni.nn.BainNN
import necc.JBox2DUtil.Vec2
import necc.ann.{ANNContext, BainNNContext}
import necc.experiment.Experiment
import necc.hyperneat.NECCFitnessFunction._
import necc.simulation.Block
import necc.task._
import org.jbox2d.common.MathUtils
import org.jgapcustomised.{ChromosomeMaterial, Chromosome}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

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


trait NECCFitnessFunction extends BulkFitnessFunctionMT {
  var rep = defaultRep
  var settings = TaskSettings.defaults
  val layers = Seq(0, 1, 2)
  var evalCollector: Option[ActorRef] = None
  implicit val timeout = Timeout(Duration(30, TimeUnit.SECONDS))

  override def init(props: Properties): Unit = {
    rep = props.getIntProperty("evaluate.averaging.repetitions", defaultRep)
    settings = TaskSettings.fromProps(props)
    val (layerWidths, layerHeights) = layers.map(getLayerDimensions(_, 3)).map(d => (d(0), d(1))).unzip
    props.setProperty("ann.hyperneat.width", layerWidths.mkString(", "))
    props.setProperty("ann.hyperneat.height", layerHeights.mkString(", "))
    val futureRef = necc.actorSystem.actorSelection(props.getProperty("collector.path")).resolveOne()
    evalCollector = Some(Await.result(futureRef, timeout.duration))
    super.init(props)
  }

  override def initialiseEvaluation(): Unit = {}

  var e = 0

  override def evaluate(genotype: Chromosome, substrate: Activator, evalThreadIndex: Int): Double = {
    substrate match {
      case ann: BainNN =>
        val annContext = new BainNNContext(ann)
        val blockLayout = BlockLayout.generate(settings.blockTypes, n = 20, min = 18f, max = 34f)
        var i = 0
        var fitness: Double = 0.0
        while (i < rep) {
          if (i > 0) annContext.reset()
          val runFitness = experiment.evaluate(settings, annContext, blockLayout)
          fitness += runFitness
          //println(s"evaluation #$e-$i: runFitness = $runFitness, fitness = $fitness")
          i += 1
        }
        fitness = (fitness / rep.toDouble) max 0.0 min 1.0
        genotype.setPerformanceValue(fitness)
        //todo: penalize fitness (not performance) by time/distance/...

        e += 1

        for (ec <- evalCollector) ec ! Evaluation(genotype.cloneMaterial(), ann, fitness)

        fitness
      case _ =>
        println("no BainNN")
        0
    }
  }

  def experiment: Experiment
}


