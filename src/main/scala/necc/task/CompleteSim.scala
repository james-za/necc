package necc.task

import necc.JBox2DUtil.Vec2
import necc.simulation.Block.Type
import necc.simulation.{Agent, Block}
import org.jbox2d.callbacks.{ContactImpulse, ContactListener}
import org.jbox2d.collision.Manifold
import org.jbox2d.common.MathUtils
import org.jbox2d.dynamics._
import org.jbox2d.dynamics.contacts.Contact

import scala.util.Random

class CompleteSim(
  val settings: TaskSettings,
  externalWorld: Option[World] = None,
  val blockLayout: Vector[(Vec2, Block.Type)]
) extends Simulation with Gathering with Construction {
  def getWorld = {
    for (w <- externalWorld) w.setGravity(Vec2.zero)
    externalWorld.getOrElse(new World(Vec2(0, 0)))
  }

  world.setContactListener(new ContactListener {
    override def beginContact(contact: Contact): Unit = beginContactLoading(contact)
    override def endContact(contact: Contact): Unit = endContactPlacing(contact)
    override def postSolve(contact: Contact, impulse: ContactImpulse): Unit = postSolvePlacing(contact)
    override def preSolve(contact: Contact, oldManifold: Manifold): Unit = ()
  })

  val agents = Vector.tabulate(5, 6) { (x: Int, y: Int) =>
    Agent(Vec2(x * 0.1f - 0.25f, y * 0.1f - 0.3f), world)
  }.flatten
  
  def maxFitness: Double = {
    val maxSlot = blocks.map(_.slots.length).sum * RewardSlotAgent
    val maxFill = blocks.length * RewardFillBlock
    val maxPlace = blocks.length * RewardPlaceBlock
    maxSlot + maxFill + maxPlace
  }
  def minFitness: Double = 0

  override def isValidPlacement(bt1: Type, edge1: Array[Vec2], bt2: Type, edge2: Array[Vec2]): Boolean = {
    true
  }

  override def stepBegin(dt: Float): Unit = {
    if (externalWorld.isEmpty) world.step(dt, 3, 8)
  }
  override def stepEnd(dt: Float): Unit = ()

  override def blockFull(block: Block): Unit = ()
}