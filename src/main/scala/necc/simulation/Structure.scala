package necc.simulation

import org.jbox2d.dynamics._
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.common.{MathUtils, Vec2}
import scala.collection.mutable.ArrayBuffer
import Structure._

class Structure(
  val body: Body
) {
  val sections = ArrayBuffer[Section]()

  def addBlock(block: Block, position: Vec2, angle: Float, world: World): Unit = {
    val shape = block.blockType.shape(position, angle + MathUtils.PI)
    fixtureDef.shape = shape

    val n = block.slots.count(_.agent.nonEmpty)
    world.destroyBody(block.body)
    fixtureDef.filter.categoryBits = if(n > 0) Category.unloadingBlock else Category.structure
    val newFixture = body.createFixture(fixtureDef)
    val subStructure = new Section(newFixture, this, shape, block.blockType, position, n)
    sections += subStructure
    if(n > 0) {
      block.slots.foreach(slot => slot.agent.foreach {
        agent =>
          val position = block.body.getWorldPoint(slot.pos)
          agent.restore(position, world)
      })
      fixtureDefSensor.shape = shape
      val sensor = body.createFixture(fixtureDefSensor)
      sensor.setUserData(subStructure)
    }
  }
}

object Structure {
  var id: Long = 0
  def nextName() = { id += 1; "Structure" + id }

  val bodyDef = new BodyDef
  bodyDef.`type` = BodyType.STATIC

  val fixtureDef = new FixtureDef
  def resetFixtureDef() = {
    fixtureDef.density = 1f
    fixtureDef.restitution = 0f
    fixtureDef.friction = 1.0f
    fixtureDef.filter.categoryBits = Category.structure
    fixtureDef.filter.maskBits = Category.all
  }
  resetFixtureDef()

  val fixtureDefSensor = new FixtureDef
  fixtureDefSensor.density = 1f
  fixtureDefSensor.restitution = 0f
  fixtureDefSensor.friction = 1.0f
  fixtureDefSensor.filter.categoryBits = Category.unloadingSensor
  fixtureDefSensor.filter.maskBits = Category.unloadingAgent
  fixtureDefSensor.isSensor = true

  val defaultPosition: Vec2 = new Vec2(0, 0)

  def apply(position: Vec2, world: World): Structure = {
    resetFixtureDef()
    bodyDef.position = position
    val body = world.createBody(bodyDef)
    val shape = new PolygonShape
    shape.setAsBox(0.5f, 0.5f)
    fixtureDef.shape = shape
    val structure = new Structure(body)
    structure.sections += new Section(body.createFixture(fixtureDef), structure, shape, Block.TypeInit, position)
    body.setUserData(structure)
    structure
  }

  def apply(world: World): Structure = apply(defaultPosition, world)

  class Section(
                 val fixture: Fixture,
                 val structure: Structure,
                 shape: PolygonShape,
                 val blockType: Block.Type,
                 val position: Vec2,
                 var n: Int = 0
                 ) {
    val edges = (shape.getVertices.take(shape.getVertexCount) :+ shape.getVertex(0)).sliding(2).toVector
    fixture.setUserData(this)
  }
}