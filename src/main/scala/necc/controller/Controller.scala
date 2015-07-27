package necc.controller

import necc.ann.ANNContext
import necc.task.Simulation

trait Controller {
  def run(task: Simulation): Unit
}

object Controller {

  class Test(annContext: ANNContext) extends Controller {
    override def run(task: Simulation): Unit = {
      for (agent <- task.agents) {
        val outputs = annContext.apply(Array.empty[Double])
        agent.setVelocity(outputs(0).toFloat, outputs(1).toFloat * 2f - 1)
      }
    }
  }

  class Gathering(val load: ANNContext) extends Controller {
    override def run(task: Simulation): Unit = {
      val agents = task.agents.filterNot(_.slotted)
      val blocks = task.blocks.filterNot(_.connected).filterNot(_.full)
      Radial.runRadial(load, agents, blocks) {
        case (agent, lin, ang) => agent.setVelocity(lin.toFloat, ang.toFloat * 2f - 1)
      }
    }
  }

  class Construction(val place: ANNContext) extends Controller {
    override def run(task: Simulation): Unit = {
      val blocks = task.blocks.filterNot(_.connected).filter(_.full)
      val sections = task.mainStructure.sections
      Radial.runRadial(place, blocks, sections) {
        case (block, lin, ang) => block.setVelocity(lin.toFloat, ang.toFloat * 2f - 1)
      }
    }
  }

  class Complete(val load: ANNContext, val place: ANNContext) extends Controller {
    override def run(task: Simulation): Unit = {
      val agents = task.agents.filterNot(_.slotted)
      val (blocksFull, blocks) = task.blocks.filterNot(_.connected).partition(_.full)
      val sections = task.mainStructure.sections
      Radial.runRadial(load, agents, blocks) {
        case (agent, lin, ang) => agent.setVelocity(lin.toFloat, ang.toFloat * 2f - 1)
      }
      Radial.runRadial(place, blocksFull, sections) {
        case (block, lin, ang) => block.setVelocity(lin.toFloat, ang.toFloat * 2f - 1)
      }
    }
  }

}