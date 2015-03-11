package necc.task

import necc.JBox2DUtil._
import necc.simulation.Block.Type
import necc.simulation.{Block, Structure, Agent}
import org.jbox2d.callbacks.{ContactImpulse, ContactListener}
import org.jbox2d.collision.Manifold
import org.jbox2d.common.MathUtils
import org.jbox2d.dynamics._
import org.jbox2d.dynamics.contacts.Contact

import scala.util.Random

class ConstructionSim(
  val settings: TaskSettings,
  externalWorld: Option[World] = None,
  val blockLayout: Vector[(Vec2, Block.Type)]
) extends Simulation with Construction with HasTaskSettings {
  def getWorld = {
    for (w <- externalWorld) w.setGravity(Vec2.zero)
    externalWorld.getOrElse(new World(Vec2(0, 0)))
  }

  world.setContactListener(new ContactListener {
    override def beginContact(contact: Contact): Unit = ()
    override def endContact(contact: Contact): Unit = endContactPlacing(contact)
    override def postSolve(contact: Contact, impulse: ContactImpulse): Unit = postSolvePlacing(contact)
    override def preSolve(contact: Contact, oldManifold: Manifold): Unit = ()
  })


  override def addBlock(structure: Structure, block: Block, position: Vec2, angle: Float): Unit = {
    block.slots.foreach(_.agent = None)
    super.addBlock(structure, block, position, angle)
  }

  for (b <- blocks; s <- b.slots) {
    Agent(world).doSlot(b, s, world)
  }

  val agents = Vector[Agent]()

  override def maxFitness: Double = {
    val maxPlace = blocks.size * RewardPlaceBlock
    maxPlace
  }

  override def minFitness: Double = 0
  
  override def stepBegin(dt: Float): Unit =
    if (externalWorld.isEmpty) world.step(dt, 3, 8)
  override def stepEnd(dt: Float): Unit = ()

  override def isValidPlacement(t1: Type, e1: Array[Vec2], t2: Type, e2: Array[Vec2]): Boolean = {
    true
  }

}
