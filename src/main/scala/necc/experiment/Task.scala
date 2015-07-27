package necc.experiment

import necc.JBox2DUtil.Vec2
import necc.ann.{TestANNContext, ANNContext}
import necc.controller._
import necc.simulation.Block
import necc.task._
import org.jbox2d.dynamics.World

sealed trait Task {
    def name: String
    def simulation(settings: TaskSettings, blockLayout: Vector[(Vec2, Block.Type)], world: Option[World] = None): Simulation
    override def toString = name
    
    def evaluate(settings: TaskSettings, controller: Controller, blockLayout: Vector[(Vec2, Block.Type)]): Double = {
      val sim = simulation(settings, blockLayout)
      var t = 0f
      var tANN = 0f

      while(t < settings.maxTime) {
        if (tANN >= settings.intervalRunANN) {
          controller.run(sim)
          tANN -= settings.intervalRunANN
        }

        sim.step(settings.intervalStep)
        t += settings.intervalStep
        tANN += settings.intervalStep
      }

      (sim.fitness - sim.minFitness) / (sim.maxFitness - sim.minFitness)
    }
  }
  object Task {
    case object Gathering extends Task {
      def name = "Gathering"
      def simulation(settings: TaskSettings, blockLayout: Vector[(Vec2, Block.Type)], world: Option[World] = None) = new GatheringSim(settings, world, blockLayout)
    }
    case object Construction extends Task {
      def name = "Construction"
      def simulation(settings: TaskSettings, blockLayout: Vector[(Vec2, Block.Type)], world: Option[World] = None) = new ConstructionSim(settings, world, blockLayout)
    }
    case object Complete extends Task {
      def name = "Complete"
      def simulation(settings: TaskSettings, blockLayout: Vector[(Vec2, Block.Type)], world: Option[World] = None) = new CompleteSim(settings, world, blockLayout)
    }
  }