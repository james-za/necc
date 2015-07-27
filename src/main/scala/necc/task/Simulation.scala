package necc.task

import necc.Consumable
import necc.JBox2DUtil.Vec2
import necc.simulation.{Agent, Block, Structure}
import org.jbox2d.collision.shapes.PolygonShape
import org.jbox2d.common.MathUtils
import org.jbox2d.dynamics._
import org.jbox2d.dynamics.contacts.Contact

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.util.Random

trait Simulation {
  val settings: TaskSettings
  val id = { Simulation.nextId += 1; Simulation.nextId }
  var alreadyRun: Boolean = false
  var fitness = 0.0
  def maxFitness: Double
  def minFitness: Double

  def getWorld: World
  val world: World = getWorld

  val random = new Random()

  val newJoints = Consumable[(Structure, Block, Vec2, Float)]()
  val oldSensors = Consumable[Fixture]()

//  val blockTypeCounts: Map[Block.Type, Int] = Map(
//    Block.TypeA -> 10,
//    Block.TypeB -> 10)
//  val blockTypesInit = blockTypeCounts.flatMap { case (bt, count) => Seq.fill(count)(bt) }.toVector
  //val blockTypes = Random.shuffle(blockTypesInit)
  val blockTypes = Vector.fill(20)(settings.blockTypes((math.random * settings.blockTypes.size).toInt))

  val blockLayout: Vector[(Vec2, Block.Type)]
  val blocks = for ((pos, bt) <- blockLayout) yield Block(bt, pos, world)
  val agents: Vector[Agent]

  val mainStructure = Structure(Vec2.zero, world)

  val boundary = Simulation.createBoundary(world, width = 100, height = 100)

  def step(dt: Float): Unit = {
    stepBegin(dt)

    for(a <- agents) {
      if (a.slotted)
        world.destroyBody(a.body)
      a.addNewSlotFixtures()
      a.applyVelocity()
    }

    for(b <- blocks) b.applyVelocity()

    for((structure, block, position, angle) <- newJoints.consumeAll)
      addBlock(structure, block, position, angle)
    for(f <- oldSensors.consumeAll) f.getBody.destroyFixture(f)

    stepEnd(dt)
  }

  def addBlock(structure: Structure, block: Block, position: Vec2, angle: Float): Unit = {
    structure.addBlock(block, position, angle, world)
  }

  def stepBegin(dt: Float): Unit
  def stepEnd(dt: Float): Unit

  implicit val vectorCBFMap = new CanBuildFrom[Map[Block.Type, Int], Block, Vector[Block]] {
    override def apply(from: Map[Block.Type, Int]): mutable.Builder[Block, Vector[Block]] = Vector.newBuilder[Block]
    override def apply(): mutable.Builder[Block, Vector[Block]] = Vector.newBuilder[Block]
  }
}

object Simulation {
  var nextId = 0

  val random = new Random
  def createBoundary(world: World, width: Float, height: Float, wallDepth: Float = 5, obstacles: Int = 0): Body = {
    val boundaryDef = new BodyDef
    boundaryDef.`type` = BodyType.STATIC
    val body = world.createBody(boundaryDef)
    val boundaryShape = new PolygonShape()
    def addBoundary(x: Float, y: Float, hx: Float, hy: Float) = {
      boundaryShape.setAsBox(hx, hy, new Vec2(x, y), 0)
      body.createFixture(boundaryShape, 1)
    }
    val offsetX = (width + wallDepth) / 2
    val offsetY = (height + wallDepth) / 2
    addBoundary(-offsetX, 0, wallDepth / 2, height / 2 + wallDepth)
    addBoundary(offsetX, 0, wallDepth / 2, height / 2 + wallDepth)
    addBoundary(0, y = -offsetY, width / 2 + wallDepth, wallDepth / 2)
    addBoundary(0, y = offsetY, width / 2 + wallDepth, wallDepth / 2)

    for (i <- 0 until obstacles) {
      val w = 4 + random.nextFloat() * 6
      val h = 4 + random.nextFloat() * 6
      val x = (random.nextFloat() - 0.5f) * (width - w)
      val y = (random.nextFloat() - 0.5f) * (height - h)

      val numPts = 4 + random.nextInt(4)
      boundaryShape.set(
        Array.fill(numPts)(Vec2(
          x + (random.nextFloat() - 0.5f) * w,
          y + (random.nextFloat() - 0.5f) * h)),
        numPts)
      body.createFixture(boundaryShape, 1)
    }
    body
  }

  def categories(contact: Contact) = (
    contact.getFixtureA.getFilterData.categoryBits,
    contact.getFixtureB.getFilterData.categoryBits)
}