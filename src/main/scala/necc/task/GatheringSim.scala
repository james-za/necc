package necc.task

import necc.Consumable
import necc.JBox2DUtil._
import necc.simulation.{Agent, Block, Category}
import necc.task.Simulation._
import org.jbox2d.callbacks.{ContactImpulse, ContactListener}
import org.jbox2d.collision.Manifold
import org.jbox2d.common.MathUtils
import org.jbox2d.dynamics._
import org.jbox2d.dynamics.contacts.Contact

class GatheringSim(
  val settings: TaskSettings,
  externalWorld: Option[World] = None,
  val blockLayout: Vector[(Vec2, Block.Type)]
) extends Simulation with Gathering {
  def getWorld = {
    for (w <- externalWorld) w.setGravity(Vec2.zero)
    externalWorld.getOrElse(new World(Vec2(0, 0)))
  }

  world.setContactListener(new ContactListener {
    override def beginContact(contact: Contact): Unit = categories(contact) match {
      case (Category.block, Category.structure) => collisionBodyStructure(contact.getFixtureA.getBody)
      case (Category.structure, Category.block) => collisionBodyStructure(contact.getFixtureB.getBody)
      case _ => beginContactLoading(contact)
    }
    override def endContact(contact: Contact): Unit = ()
    override def postSolve(contact: Contact, impulse: ContactImpulse): Unit = ()
    override def preSolve(contact: Contact, oldManifold: Manifold): Unit = ()
  })

  val oldBodies = Consumable[Body]()

  def collisionBodyStructure(body: Body): Unit = {
    oldBodies += body
  }

  var fullBlocks = Set[Body]()

  val agents = Vector.tabulate(5, 6) { (x: Int, y: Int) =>
    Agent(Vec2(x * 0.1f - 0.25f, y * 0.1f - 0.3f), world)
  }.flatten

  override def maxFitness: Double = {
    val maxSlot = blocks.map(_.slots.size).sum * RewardSlotAgent
    val maxFill = blocks.size * RewardFillBlock
    maxSlot + maxFill
  }

  override def minFitness: Double = 0

  override def blockFull(block: Block): Unit = fullBlocks += block.body

  override def stepBegin(dt: Float): Unit = {
    oldBodies.consumeAll { b =>
      world.destroyBody(b)
      fullBlocks -= b
    }
    for (b <- fullBlocks) aimBodyAtStructure(b)
    if (externalWorld.isEmpty) world.step(dt, 3, 8)
  }
  override def stepEnd(dt: Float): Unit = {}

  def aimBodyAtStructure(body: Body): Unit = {
    val pos = body.getPosition
    val dir = mainStructure.body.getPosition - pos
    val angle = MathUtils.atan2(dir.y, dir.x)
    body.setTransform(pos, angle)
  }
}
