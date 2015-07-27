package necc.testbed

import necc.ann.ANNContext
import necc.controller.{Radial, Controller}
import necc.experiment.Task
import necc.task._
import org.jbox2d.common.Vec2
import org.jbox2d.testbed.framework.TestbedSetting.SettingType
import org.jbox2d.testbed.framework.{TestbedSetting, TestbedSettings, TestbedTest}

class NECCTest(settings: TaskSettings, et: Task, val controller: Controller) extends TestbedTest {

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
  val intervalANN = settings.intervalRunANN

  var simulation: Option[Simulation] = None

  override def initTest(deserialized: Boolean): Unit = if (!deserialized) {
    val hz: Int = (1.0f / settings.intervalStep).toInt
    timeSinceANN = 0f
    getWorld.setGravity(new Vec2(0, 0))
    val world = Some(getWorld)
    val blockLayout = BlockLayout.generate(settings.blockTypes, n = 20, min = 18f, max = 34f)

    simulation = Some(et.simulation(settings, blockLayout, world))

    //for (t <- simulation) controller.run(t)
  }

  override def step(testbedSettings: TestbedSettings): Unit = {
    super.step(testbedSettings)

    for (t <- simulation) {
      val dt = settings.intervalStep
      t.step(dt)

      if (timeSinceANN > intervalANN) {
        controller.run(t)
        timeSinceANN -= intervalANN
      }
      timeSinceANN += dt

      //println(f"$dt%5.5f | $timeSinceANN%5.5f")
    }
  }
}