package necc.testbed

import necc.ann.ANNContext
import necc.controller.{Controller, RadialLoadBlocks, RadialComplete, RadialPlaceBlocks}
import necc.experiment.Experiment.{CompleteTask, ExperimentTask, LoadBlocksTask, PlaceBlocksTask}
import necc.task._
import org.jbox2d.common.Vec2
import org.jbox2d.testbed.framework.{TestbedSettings, TestbedTest}

class NECCTest(settings: TaskSettings, et: ExperimentTask, load: ANNContext, place: ANNContext) extends TestbedTest {
  def end(): Unit = {
    var i = 0
    var list = getWorld.getBodyList
    while(list != null && i < getWorld.getBodyCount) {
      getWorld.destroyBody(list)
      list = list.getNext
      i += 1
    }
  }

  override def getTestName: String = "NECC Test"

  var timeSinceANN = 0f
  val intervalANN = 1f

  var task: Option[Simulation] = None
  var controller: Option[Controller] = None

  override def initTest(deserialized: Boolean): Unit = if (!deserialized) {
    timeSinceANN = 0f
    getWorld.setGravity(new Vec2(0, 0))
    val world = Some(getWorld)
    val blockLayout = BlockLayout.generate(settings.blockTypes, n = 20, min = 18f, max = 34f)
    et match {
      case LoadBlocksTask =>
        controller = Some(new RadialLoadBlocks(load))
        task = Some(new GatheringSim(settings, world, blockLayout))
      case PlaceBlocksTask =>
        controller = Some(new RadialPlaceBlocks(place))
        task = Some(new ConstructionSim(settings, world, blockLayout))
      case CompleteTask =>
        controller = Some(new RadialComplete(load, place))
        task = Some(new Complete(settings, world, blockLayout))
    }

    for (t <- task; c <- controller) c.run(t)
  }

  override def step(settings: TestbedSettings): Unit = {
    super.step(settings)

    for (t <- task; c <- controller) {
      val dt = 1.0f / settings.getSetting("Hz").value
      t.step(dt)

      if (timeSinceANN > intervalANN) {
        c.run(t)
        timeSinceANN -= intervalANN
      }
      timeSinceANN += dt
    }
  }
}