package necc.controller

import necc.JBox2DUtil._
import necc.simulation.Structure.Section
import necc.simulation.{Block, Agent}

trait Pos[T] {
  def position(t: T): Vec2
}

object Pos {
  implicit object PositionableAgent extends Pos[Agent] {
    override def position(agent: Agent): Vec2 = agent.body.getPosition
  }
  implicit object PositionableBlock extends Pos[Block] {
    override def position(block: Block): Vec2 = block.body.getPosition
  }
  implicit object PositionableSection extends Pos[Section] {
    override def position(section: Section): Vec2 = section.position
  }
}