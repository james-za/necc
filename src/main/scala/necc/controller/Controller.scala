package necc.controller

import necc.ann.ANNContext
import necc.task.{Simulation, Simulation$}
import org.jbox2d.common.MathUtils

trait Controller {
  def run(task: Simulation): Unit
}

class TestLoadBlocks(annContext: ANNContext) extends Controller {
  override def run(task: Simulation): Unit = {
    for(agent <- task.agents) {
      val outputs = annContext.apply(Array.empty[Double])
      agent.setVelocity(outputs(0).toFloat, outputs(1).toFloat)
    }
  }
}

class RadialLoadBlocks(load: ANNContext) extends Controller with Radial {
  override def run(task: Simulation): Unit = {
    val agents = task.agents.filterNot(_.slotted)
    val blocks = task.blocks.filterNot(_.connected).filterNot(_.full)
    runRadial(load, agents, blocks) {
      case (agent, lin, ang) => agent.setVelocity(lin.toFloat, ang.toFloat)
    }
  }
}

class RadialPlaceBlocks(place: ANNContext) extends Controller with Radial {
  override def run(task: Simulation): Unit = {
    val blocks = task.blocks.filterNot(_.connected).filter(_.full)
    val sections = task.mainStructure.sections
    runRadial(place, blocks, sections) {
      case (block, lin, ang) => block.setVelocity(lin.toFloat, ang.toFloat)
    }
  }
}

class RadialComplete(load: ANNContext, place: ANNContext) extends Controller with Radial {
  override def run(task: Simulation): Unit = {
    val agents = task.agents.filterNot(_.slotted)
    val (blocksFull, blocks) = task.blocks.filterNot(_.connected).partition(_.full)
    val sections = task.mainStructure.sections
    runRadial(load, agents, blocks) {
      case (agent, lin, ang) => agent.setVelocity(lin.toFloat, ang.toFloat)
    }
    runRadial(place, blocksFull, sections) {
      case (block, lin, ang) => block.setVelocity(lin.toFloat, ang.toFloat)
    }
  }
}

class GridLoadBlocks(load: ANNContext) extends Controller with Grid {
  override def run(task: Simulation): Unit = {
    val agents = task.agents.filterNot(_.slotted)
    val blocks = task.blocks.filterNot(_.connected).filterNot(_.full)
    runGrid(load, agents, blocks) {
      case (agent, offset) =>
        val pos = agent.body.getPosition
        val angle = MathUtils.atan2(offset.y, offset.x)
        agent.body.setTransform(pos, angle)
        agent.setVelocity(2.0f, 0.0f)
    }
  }
}

class GridPlaceBlocks(place: ANNContext) extends Controller with Grid {
  override def run(task: Simulation): Unit = {
    val blocks = task.blocks.filterNot(_.connected).filter(_.full)
    val sections = task.mainStructure.sections
    runGrid(place, blocks, sections) {
      case (block, offset) =>
        val pos = block.body.getPosition
        val angle = MathUtils.atan2(offset.y, offset.x)
        block.body.setTransform(pos, angle)
        block.setVelocity(2.0f, 0.0f)
    }
  }
}

class GridComplete(load: ANNContext, place: ANNContext) extends Controller with Grid  {
  override def run(task: Simulation): Unit = {
    val agents = task.agents.filterNot(_.slotted)
    val (blocksFull, blocks) = task.blocks.filterNot(_.connected).partition(_.full)
    val sections = task.mainStructure.sections
    runGrid(load, agents, blocks) {
      case (agent, offset) =>
        val pos = agent.body.getPosition
        val angle = MathUtils.atan2(offset.y, offset.x)
        agent.body.setTransform(pos, angle)
        agent.setVelocity(2.0f, 0.0f)
    }
    runGrid(load, blocksFull, sections) {
      case (block, offset) =>
        val pos = block.body.getPosition
        val angle = MathUtils.atan2(offset.y, offset.x)
        block.body.setTransform(pos, angle)
        block.setVelocity(2.0f, 0.0f)
    }
  }
}