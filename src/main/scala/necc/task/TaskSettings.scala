package necc.task

import com.ojcoleman.ahni.hyperneat.Properties
import necc.gui.BlockPreview
import necc.simulation.Block

trait HasTaskSettings { this: Simulation =>
  val settings: TaskSettings
}

case class TaskSettings(
  intervalStep: Float = 1f / 30f,
  intervalRunANN: Float = 1f,
  maxTime: Float = 180,
  blockTypes: Seq[Block.Type] = Seq(Block.TypeA, Block.TypeB),
  //todo: blockLayout: Seq[Vec2]
  sensors: Int = 8,
  sensorRange: Double = 50.0
) {
  assert(intervalStep < intervalRunANN)

}

object TaskSettings {
  val defaults = TaskSettings()
  def fromProps(props: Properties): TaskSettings = {
    val t = props.getFloatProperty("evaluate.duration.maximum", defaults.maxTime)
    val s = props.getFloatProperty("evaluate.interval.step", defaults.intervalStep)
    val c = props.getFloatProperty("evaluate.interval.controller", defaults.intervalRunANN)
    val bts = for {
      bt <- props.getProperty("evaluate.blocks.types", "").split("\\|").toSeq
      poly <- BlockPreview.parsePolygon(bt)
    } yield Block.TypeCustom(poly)
    val sc = props.getIntProperty("evaluate.sensors.count", defaults.sensors)
    val sr = props.getDoubleProperty("evaluate.sensors.range", defaults.sensorRange)
    TaskSettings(
      intervalStep = s,
      intervalRunANN = c,
      maxTime = t,
      blockTypes = bts,
      sensors = sc,
      sensorRange = sr
    )
  }
}