package necc

import java.text.NumberFormat

import com.ojcoleman.ahni.nn.BainNN
import jfxtras.labs.scene.control.BigDecimalField
import necc.JBox2DUtil.Vec2
import necc.experiment.Experiment.{CompleteTask, ExperimentTask, LoadBlocksTask, PlaceBlocksTask}
import org.jgapcustomised.ChromosomeMaterial
import sbinary.{DefaultProtocol, Format}

import scala.util.{Failure, Success, Try}
import scalafx.Includes._
import scalafx.beans.property.{DoubleProperty, IntegerProperty, Property, StringProperty}
import scalafx.scene.Node
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.scene.control.{Tooltip, Label, TextField}
import scalafx.scene.layout.{HBox, Priority, Region}
import scalafx.util.StringConverter
import scalafx.util.converter._

package object gui {
  object Spacer {
    def apply(): Region = {
      val spacer = new javafx.scene.layout.Region()
      HBox.setHgrow(spacer, Priority.Always)
      spacer
    }
  }

  def labeled(label: String, node: Node, spacer: Boolean) = new HBox(5) {
    content = if (spacer) Seq(Label(label), Spacer(), node) else Seq(Label(label), node)
  }
  def labeled(node: Node, label: String, spacer: Boolean) = new HBox(5) {
    content = if (spacer) Seq(node, Spacer(), Label(label)) else Seq(node, Label(label))
  }

  implicit class RichNode[N <% Node](node: N) {
    def @:(label: String) = labeled(label, node, spacer = false)
    def :@(label: String) = labeled(node, label, spacer = false)
    def @~:(label: String) = labeled(label, node, spacer = true)
    def :~@(label: String) = labeled(node, label, spacer = true)
  }
  
  implicit val strConvDouble = new DoubleStringConverter
  implicit val strConvBigDecimal = new BigDecimalStringConverter
  implicit val strConvFloat = new FloatStringConverter
  implicit val strConvInt= new IntStringConverter
  implicit val strConvBoolean = new BooleanStringConverter
  implicit val strConvLong = new LongStringConverter
  implicit val strConvDate = new DateStringConverter
  implicit val strConvDateTime = new DateTimeStringConverter
  implicit val strConvBigInteger = new BigIntStringConverter

  implicit class RichStringProperty(sp: StringProperty) {
    def <==>[T, J](prop: Property[T, J])(implicit converter: StringConverter[T]) = {

//      sp.onChange { (ov, oldVal, newVal) =>
//        try {
//          val value = converter.fromString(newVal)
//          prop.delegate.setValue(value)
//        } catch {
//          case ex: Exception => sp.value = oldVal
//        }
//      }
//      prop.onChange { (ov, oldVal, newVal) =>
//        try {
//          sp.value = converter.toString(newVal)
//        }
//      }
    }
  }

  def IntField2(default: Int) = {
    val bdf = new BigDecimalField(
      java.math.BigDecimal.valueOf(default),
      java.math.BigDecimal.ONE,
      NumberFormat.getIntegerInstance
    )
    bdf.hgrow = Priority.Always
    bdf
  }

  class IntField(default: Int) extends TextField {
    val property = IntegerProperty(default)

    val invalid = new scalafx.scene.effect.InnerShadow(6.0, "red")
    hgrow = Priority.Always

    text = default.toString
    text.onChange((_, oldVal, newVal) => {
      Try(newVal.toInt) match {
        case Failure(exception) =>
          effect = invalid
        case Success(value) =>
          property.value = value
          effect = null
      }
    })
  }

  class DoubleField(default: Double) extends TextField {
    val property = DoubleProperty(default)

    val invalid = new scalafx.scene.effect.InnerShadow(6.0, "red")
    hgrow = Priority.Always

    text = default.toString
    text.onChange((_, oldVal, newVal) => {
      Try(newVal.toDouble) match {
        case Failure(exception) =>
          effect = invalid
        case Success(value) =>
          property.value = value
          effect = null
      }
    })
  }

  class FloatField(default: Float) extends TextField {
    val property = DoubleProperty(default)

    val invalid = new scalafx.scene.effect.InnerShadow(6.0, "red")
    hgrow = Priority.Always

    text = default.toString
    text.onChange((_, oldVal, newVal) => {
      Try(newVal.toFloat) match {
        case Failure(exception) =>
          effect = invalid
        case Success(value) =>
          property.value = value
          effect = null
      }
    })
  }

  class PropertyLabel[T](name: String, property: Property[T, T])(implicit converter: StringConverter[T]) extends HBox {
    val label = new Label(name)
    val field = new TextField
    field.text.bindBidirectional[T](property, converter)
    //field.text <==> property
    content = Seq(label, field)
  }

//  def trunc(x: Double, n: Int) = {
//    def p10(n: Int, pow: Long = 10): Long = if (n==0) pow else p10(n-1,pow*10)
//    if (n < 0) {
//      val m = p10(-n).toDouble
//      math.round(x/m) * m
//    }
//    else {
//      val m = p10(n).toDouble
//      math.round(x*m) / m
//    }
//  }
  def trunc(x: Double, n: Int): Double = { val s = math pow (10, n); (math round x * s) / s }

  object RunDataProtocol extends DefaultProtocol {
    import necc.BainNNProtocol._
    import necc.ChromosomeMaterialProtocol._
    implicit lazy val RunDataFormat: Format[RunData] = asProduct3(RunData.apply)(d => (d.task, d.anns, d.runs))
    implicit lazy val RunFormat: Format[Run] = asProduct2(Run.apply)(r => (r.run, r.gens))
    implicit lazy val GenFormat: Format[Gen] = asProduct2(Gen.apply)(g => (g.gen, g.fitness))
    implicit lazy val ExperimentTaskFormat: Format[ExperimentTask] = wrap[ExperimentTask, Int]({
      case LoadBlocksTask => 0
      case PlaceBlocksTask => 1
      case CompleteTask => 2
    }, {
      case 0 => LoadBlocksTask
      case 1 => PlaceBlocksTask
      case 2 => CompleteTask
    })
  }

  case class RunData(task: ExperimentTask, anns: Set[(ChromosomeMaterial, BainNN)], runs: Array[Run])
  case class Run(run: Int, gens: Array[Gen])
  case class Gen(gen: Int, fitness: Double)

  case class Poly(points: Array[Vec2], agents: Int) {
    override def toString: String =
      agents + "#" + points.map(p => s"${p.x},${p.y}").mkString(", ")
  }
}
