package necc.task

import necc.simulation.{Block, Agent, Category}
import org.jbox2d.dynamics.Fixture
import org.jbox2d.dynamics.contacts.Contact
import Simulation._

trait Gathering { this: Simulation with Gathering =>
  val RewardSlotAgent = 1.0
  val RewardFillBlock = 1.0

  def beginContactLoading(contact: Contact): Unit = categories(contact) match {
    case (Category.agent, Category.block) => collisionAgentBlock(contact.getFixtureA, contact.getFixtureB)
    case (Category.block, Category.agent) => collisionAgentBlock(contact.getFixtureB, contact.getFixtureA)
    case _ =>
  }

  def collisionAgentBlock(fixtureAgent: Fixture, fixtureBlock: Fixture): Unit = {
    val agent = fixtureAgent.getBody.getUserData.asInstanceOf[Agent]
    val block = fixtureBlock.getBody.getUserData.asInstanceOf[Block]
    if (!agent.slotted) {
      if (agent.trySlot(block, world)) {
        fitness += RewardSlotAgent
        if(block.full) {
          fitness += RewardFillBlock
          blockFull(block)
          block.setVelocity(2, 0)
        }
      }
    }
  }

  def blockFull(block: Block): Unit
}
