package necc.simulation

import org.jbox2d.dynamics._
import org.jbox2d.collision.shapes.CircleShape
import Agent._
import scala.collection.mutable.ArrayBuffer
import necc.JBox2DUtil._
import scala.Some
import necc.simulation.Block.Slot

class Agent(
  val name: String,
  var body: Body,
  var fixture: Fixture
) {
  var slotted = false
  var linearVelocity = 0f//Vec2(0, 0)
  var angularVelocity = 0f
  
  def addNewSlotFixtures(): Unit = {
    newSlotFixtures.foreach {
      case (block, slot) =>
        fixtureDefSlot.shape = shape(slot.pos, (math.Pi / 2).toFloat)
        slot.fixture = Some(block.body.createFixture(fixtureDefSlot))
    }
    newSlotFixtures.clear()
  }
  
  def restore(position: Vec2, world: World): Unit = {
    bodyDef.position = position
    body = world.createBody(bodyDef)
    body.setUserData(this)
    body.setLinearDamping(5.0f)
    body.setAngularDamping(10.0f)
    fixture = body.createFixture(fixtureDef)
    fixture.setFriction(0)
    slotted = false
    fixture.getFilterData.categoryBits = Category.unloadingAgent
    fixture.getFilterData.maskBits = Category.all ^ Category.unloadingBlock // no collision with block being unloaded
    fixture.refilter()
  }

  def setVelocity(linVel: Float, angVel: Float) {
    //linearVelocity = body.getWorldVector(new Vec2(0f, linVel))
    linearVelocity = linVel
    angularVelocity = angVel
  }

  def applyVelocity() {
    body.setLinearVelocity(body.getWorldVector(new Vec2(linearVelocity, 0f)))
    body.setAngularVelocity(angularVelocity)
  }

  def trySlot(block: Block, world: World): Boolean = {
    block.slots
      .find(_.agent.isEmpty)
      .map(doSlot(block, _, world))
      .isDefined
  }

  val fixtureDefSlot = new FixtureDef

  val newSlotFixtures = ArrayBuffer[(Block, Slot)]()

  def doSlot(block: Block, slot: Slot, world: World): Unit = {
    slot.agent = Some(this)
    slotted = true
    world.destroyBody(body)
    newSlotFixtures += ((block, slot))
  }
}

object Agent {
  var id: Long = 0
  def nextName() = { id += 1; "Agent" + id }

  val bodyDef = new BodyDef
  bodyDef.`type` = BodyType.DYNAMIC


  def shape(position: Vec2, angle: Float) = {
//    val shape = new PolygonShape
//    shape.setAsBox(0.3f, 0.3f, position, angle)
    val shape = new CircleShape
    shape.setRadius(0.4f)
    shape.m_p.set(position)
    shape
  }

  val fixtureDef = new FixtureDef
  fixtureDef.shape = shape(Vec2(0, 0), 0f)
  fixtureDef.density = 1f
  fixtureDef.restitution = 0f
  fixtureDef.friction = 1.0f
  fixtureDef.filter.categoryBits = Category.agent
  fixtureDef.filter.maskBits = Category.all

  val defaultPosition: Vec2 = new Vec2(0, 0)

  def apply(position: Vec2, world: World): Agent = {
    bodyDef.position = position
    val body = world.createBody(bodyDef)
    val fixture = body.createFixture(fixtureDef)
    fixture.setFriction(0)
    val agent = new Agent(nextName(), body, fixture)
    body.setUserData(agent)
    body.setLinearDamping(5.0f)
    body.setAngularDamping(10.0f)
    agent
  }

  def apply(world: World): Agent = apply(defaultPosition, world)
}