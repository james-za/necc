package necc.experiment

import necc.JBox2DUtil.Vec2
import necc.ann.ANNContext
import necc.controller.{GridLoadBlocks, RadialLoadBlocks, RadialPlaceBlocks, _}
import necc.experiment.Experiment._
import necc.simulation.Block
import necc.task._

case class Experiment(encoding: Encoding, et: ExperimentTask) {
  def evaluate(settings: TaskSettings, annContext: ANNContext, blockLayout: Vector[(Vec2, Block.Type)]): Double = {
    val task = et match {
      case LoadBlocksTask => new GatheringSim(settings, None, blockLayout)
      case PlaceBlocksTask => new ConstructionSim(settings, None, blockLayout)
      case CompleteTask => new Complete(settings, None, blockLayout)
    }
    val controller = (encoding, et) match {
      case (RadialEncoding, LoadBlocksTask) => new RadialLoadBlocks(annContext)
      case (RadialEncoding, PlaceBlocksTask) => new RadialPlaceBlocks(annContext)
      case (RadialEncoding, CompleteTask) => new RadialComplete(annContext, annContext)
      case (GridEncoding, LoadBlocksTask) => new GridLoadBlocks(annContext)
      case (GridEncoding, PlaceBlocksTask) => new GridPlaceBlocks(annContext)
      case (GridEncoding, CompleteTask) => new GridComplete(annContext, annContext)
    }
    var t = 0f
    var tANN = 0f

    while(t < settings.maxTime) {
      if (tANN >= settings.intervalRunANN) {
        controller.run(task)
        tANN -= settings.intervalRunANN
      }

      task.step(settings.intervalStep)
      t += settings.intervalStep
      tANN += settings.intervalStep
    }

    (task.fitness - task.minFitness) / (task.maxFitness - task.minFitness)
  }
}

object Experiment {
  sealed trait Encoding {
    def name: String
    override def toString = name
  }
  case object RadialEncoding extends Encoding {
    def name = "Radial"
  }
  case object GridEncoding extends Encoding {
    def name = "Grid"
  }

  sealed trait ExperimentTask {
    def name: String
    override def toString = name
  }
  case object LoadBlocksTask extends ExperimentTask {
    def name = "Load"
  }
  case object PlaceBlocksTask extends ExperimentTask {
    def name = "Place"
  }
  case object CompleteTask extends ExperimentTask {
    def name = "Complete"
  }

  val RadialLoadBlocks = new Experiment(RadialEncoding, LoadBlocksTask)
  val RadialPlaceBlocks = new Experiment(RadialEncoding, PlaceBlocksTask)
  val RadialComplete = new Experiment(RadialEncoding, CompleteTask)
  val GridLoadBlocks = new Experiment(GridEncoding, LoadBlocksTask)
  val GridPlaceBlocks = new Experiment(GridEncoding, PlaceBlocksTask)
  val GridComplete = new Experiment(GridEncoding, CompleteTask)
}