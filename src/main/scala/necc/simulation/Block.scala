package necc.simulation

import necc.gui.Poly
import org.jbox2d.dynamics._
import org.jbox2d.collision.shapes.PolygonShape
import necc.JBox2DUtil._
import necc.simulation.Block.Slot
import necc.simulation.Structure.Section

class Block(
  val body: Body,
  val fixture: Fixture,
  val blockType: Block.Type
) {

  var connected = false
  var linearVelocity = 0f
  var angularVelocity = 0f

  val shape = blockType.shape(new Vec2(0, 0), 0.0f)
  val slots: Seq[Slot] = blockType.slotPositions.map(Slot)

  val edges = (shape.getVertices.take(shape.getVertexCount) :+ shape.getVertex(0)).sliding(2).toVector

  def full: Boolean = slots.forall(_.agent.nonEmpty)

  def setVelocity(linVel: Float, angVel: Float) {
    linearVelocity = linVel
    angularVelocity = angVel
  }

  def applyVelocity() {
    body.setLinearVelocity(body.getWorldVector(new Vec2(linearVelocity, 0f)))
    body.setAngularVelocity(angularVelocity)
  }
  def tryConnect(subStructure: Section, world: World) = {
    val connections = for {
      edge1 <- edges
      edge2 <- subStructure.edges
      ds <- Block.canEdgesConnect(
        edge1 map body.getWorldPoint,
        edge2 map subStructure.structure.body.getWorldPoint)
    } yield ((blockType, edge1, subStructure.blockType, edge2), ds)
    if(connections.isEmpty) None else Some(connections.minBy(_._2)._1)
  }
}

object Block {

  val maxConnectDist = 1.0f
  val distThreshold = maxConnectDist * maxConnectDist * 4f
  val angleThreshold = math.Pi / 3.0

  case class Slot(pos: Vec2) {
    var agent: Option[Agent] = None
    var fixture: Option[Fixture] = None
  }

  sealed trait Type {
    def shape(position: Vec2, angle: Float): PolygonShape
    val slotPositions: Seq[Vec2]
  }
  case object TypeA extends Type {
    override val slotPositions = Vector(
      new Vec2(0, 0))
    def shape(position: Vec2, angle: Float) = {
      val shape = new PolygonShape
      shape.setAsBox(0.5f, 0.5f, position, angle)
      shape
    }
  }
  case object TypeB extends Type {
    override val slotPositions = Vector(
      new Vec2(-0.5f, 0),
      new Vec2(0.5f, 0))
    def shape(position: Vec2, angle: Float) = {
      val shape = new PolygonShape
      shape.setAsBox(1.0f, 0.5f, position, angle)
      shape
    }
  }
  case object TypeInit extends Type {
    override val slotPositions = Vector.empty
    def shape(position: Vec2, angle: Float) = {
      val shape = new PolygonShape
      shape.setAsBox(0.5f, 0.5f, position, angle)
      shape
    }
  }
  case class TypeCustom(poly: Poly) extends Type {
    override val slotPositions = Vector.fill(poly.agents)(Vec2(0, 0))
    def shape(position: Vec2, angle: Float) = {
      val shape = new PolygonShape
      val points = poly.points.map(p => {
        if (angle == 0f) p add position else
          org.jbox2d.common.Mat22.createRotationalTransform(angle).mul(p).addLocal(position)
      })
      shape.set(points, points.size)
      shape
    }
  }

  def canEdgesConnect(e0: Array[Vec2], e1: Array[Vec2]): Option[Float] = {
    val s1 = e1(0).add(e1(1))
    val s0 = e0(0).add(e0(1))
    val ds = s1.sub(s0).lengthSquared()
    if (ds < distThreshold && {
      val v0 = e0(1).sub(e0(0)); v0.normalize()
      val v1 = e1(1).sub(e1(0)); v1.normalize()
        val dot = v0 dot v1
        val t = math.acos(dot)
      val an = math.abs(t - math.Pi)
      an < angleThreshold
    }) Some(ds) else None
  }

  var id: Long = 0
  def nextName() = { id += 1; "Block" + id }

  val bodyDef = new BodyDef
  bodyDef.`type` = BodyType.DYNAMIC

  val shape = new PolygonShape
  shape.setAsBox(0.5f, 0.5f)

  val fixtureDef = new FixtureDef
  fixtureDef.shape = shape
  fixtureDef.density = 1f
  fixtureDef.restitution = 0f
  fixtureDef.friction = 1.0f
  fixtureDef.filter.categoryBits = Category.block
  fixtureDef.filter.maskBits = Category.all

  val defaultPosition: Vec2 = new Vec2(0, 0)

  def apply(blockType: Type, position: Vec2, world: World): Block = {
    bodyDef.position = position
    val body = world.createBody(bodyDef)
    fixtureDef.shape = blockType.shape(new Vec2(0, 0), 0.0f)
    val fixture = body.createFixture(fixtureDef)
    fixture.setFriction(0)
    val block = new Block(body, fixture, blockType)
    body.setUserData(block)
    body.setLinearDamping(4.0f)
    body.setAngularDamping(10.0f)
    block
  }

  def apply(blockType: Type, world: World): Block = apply(blockType, defaultPosition, world)
}
