package necc.controller

import necc.simulation.{Block, Agent}

trait Dir[T] {
  def facing(t: T): Float
}

object Dir {
  implicit object PositionableAgent extends Dir[Agent] {
    override def facing(agent: Agent): Float = agent.body.getAngle
  }
  implicit object PositionableBlock extends Dir[Block] {
    override def facing(block: Block): Float = block.body.getAngle
  }
}
