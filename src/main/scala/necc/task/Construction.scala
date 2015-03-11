package necc.task

import necc.JBox2DUtil._
import necc.simulation.{Category, Block}
import necc.simulation.Structure.Section
import org.jbox2d.common.{Mat22, MathUtils}
import org.jbox2d.dynamics.Fixture
import org.jbox2d.dynamics.contacts.Contact
import Simulation._

trait Construction { this: Simulation =>
  val RewardPlaceBlock = 10.0

  def endContactPlacing(contact: Contact): Unit = categories(contact) match {
    case (Category.unloadingAgent, Category.unloadingSensor) => unloadingAgentLeaveSensor(contact.getFixtureA, contact.getFixtureB)
    case (Category.unloadingSensor, Category.unloadingAgent) => unloadingAgentLeaveSensor(contact.getFixtureB, contact.getFixtureA)
    case _ =>
  }

  def postSolvePlacing(contact: Contact): Unit = categories(contact) match {
    case (Category.block, Category.structure) => collisionBlockStructure(contact.getFixtureA, contact.getFixtureB)
    case (Category.structure, Category.block) => collisionBlockStructure(contact.getFixtureB, contact.getFixtureA)
    case _ =>
  }

  def collisionBlockStructure(fixtureBlock: Fixture, fixtureStructure: Fixture): Unit = {
    val block = fixtureBlock.getBody.getUserData.asInstanceOf[Block]
    if(!block.connected) {
      val subStructure = fixtureStructure.getUserData.asInstanceOf[Section]
      block.tryConnect(subStructure, world).collect {
        case (t1, edgeBlock, t2, edgeStructure) if isValidPlacement(t1, edgeBlock, t2, edgeStructure) =>
          val angleBlock = MathUtils.atan2(
            edgeBlock(1).y - edgeBlock(0).y,
            edgeBlock(1).x - edgeBlock(0).x)
          val angleStructure = MathUtils.atan2(
            edgeStructure(1).y - edgeStructure(0).y,
            edgeStructure(1).x - edgeStructure(0).x)
          val angle = angleStructure - angleBlock
          val midBlock = edgeBlock(0).add(edgeBlock(1)).mulLocal(0.5f)
          val midStructure = edgeStructure(0).add(edgeStructure(1)).mulLocal(0.5f)
          val midBlockRot = Mat22.createRotationalTransform(angle + MathUtils.PI).mul(midBlock)
          val position = midStructure.sub(midBlockRot)
          newJoints += (subStructure.structure, block, position, angle)
          block.connected = true
          fitness += RewardPlaceBlock
      }
    }
  }

  def unloadingAgentLeaveSensor(fixtureAgent: Fixture, fixtureSensor: Fixture): Unit = {
    fixtureAgent.getFilterData.categoryBits = Category.agent
    fixtureAgent.getFilterData.maskBits = Category.all
    fixtureAgent.refilter()
    val subStructure = fixtureSensor.getUserData.asInstanceOf[Section]
    subStructure.n -= 1
    if(subStructure.n == 0) {
      oldSensors += fixtureSensor
      subStructure.fixture.getFilterData.categoryBits = Category.structure
      subStructure.fixture.refilter()
    }
  }

  def isValidPlacement(bt1: Block.Type, edge1: Array[Vec2], bt2: Block.Type, edge2: Array[Vec2]): Boolean
}
