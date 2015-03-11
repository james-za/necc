package necc.gui

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType
import scalafx.scene.text.Font
import scalafx.Includes._

class SensorCanvas extends Canvas(300, 300) {
  def paint(n: Int, range: Double, rAgent: Double, rSensor: Double): Unit = {
    val wSensor = (2 * math.Pi * rAgent / n) * 0.4
    val g = graphicsContext2D
    g.fill = Color.White
    g.stroke = Color.Black
    g.clearRect(0, 0, 300, 300)
    if (n > 1) {
      g.save()
      g.translate(150, 150)

      g.lineWidth = 1

      val extent = 360.0 / n
      val h = extent / 2
      for (i <- 0 until n) {
        g.save()
        g.rotate(-i * extent - h)

        g.stroke = Color.DimGrey
        g.fill = Color.LightGrey
        g.strokeArc(-rSensor, -rSensor, rSensor * 2, rSensor * 2, -h, extent, ArcType.Round)
        g.fillArc(-rSensor, -rSensor, rSensor * 2, rSensor * 2, -h, extent, ArcType.Round)

        g.stroke = Color.Black
        g.fill = Color.White
        g.beginPath()
        g.rect(rAgent - 4, -wSensor / 2, 8, wSensor)
        g.fillPath()
        g.strokePath()

        if (i == 0) {
          g.save()
          g.fill = Color.Black

          val x1 = rAgent + 4 + 2
          val x2 = rSensor - 2

          g.font = Font.default
          g.fillText("r = " + range, x1 + 20, -3)

          g.strokeLine(x1, 0, x1 + 3, 3)
          g.strokeLine(x1, 0, x1 + 3, -3)
          g.strokeLine(x2, 0, x2 - 3, 3)
          g.strokeLine(x2, 0, x2 - 3, -3)

          g.strokeLine(x1, 0, rSensor - 2, 0)

          g.restore()
        }

        g.restore()
      }

      g.beginPath()
      g.arc(0, 0, rAgent, rAgent, 0, 360)
      g.fillPath()
      g.strokePath()

      val xe = rAgent - 5
      g.lineWidth = 2
      g.strokeLine(10, 0, xe, 0)
      g.strokeLine(xe, 0, xe - 4, 4)
      g.strokeLine(xe, 0, xe - 4, -4)

      g.restore()
    }
  }
}
