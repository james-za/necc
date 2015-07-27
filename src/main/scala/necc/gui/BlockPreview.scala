package necc.gui

import necc.JBox2DUtil._
import BlockPreview._

import scala.util.Try
import scalafx.Includes._
import scalafx.geometry.{Rectangle2D, Point2D}
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.ListView.EditEvent
import scalafx.scene.control.cell.TextFieldListCell
import scalafx.scene.control.{ListView, Label, Button}
import scalafx.scene.input.{MouseButton, MouseEvent}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.ArcType
import scalafx.util.StringConverter

class BlockPreview extends VBox {

  val polys = Seq(
    Poly(Array(Vec2(-0.5f,-0.5f), Vec2(0.5f,-0.5f), Vec2(0.5f,0.5f), Vec2(-0.5f,0.5f)), 1),
    Poly(Array(Vec2(-1,-0.5f), Vec2(1,-0.5f), Vec2(1,0.5f), Vec2(-1,0.5f)), 2),
    Poly(Array(), 0)
  )
  val blockTypePane = new ListView[Poly](polys) {
    editable = true
    cellFactory = _ => new PolyListCell
    onEditCommit = (e: EditEvent[Poly]) => {
      if (e.newValue.points.size > 0) {
        val lastIndex = e.source.items().size() - 1
        val oldValue = e.source.items().get(e.index)
        if (e.index == lastIndex && oldValue.points.isEmpty)
          items() += Poly(Array(), 0)
        e.source.items().set(e.index, e.newValue)
      }
    }
    prefHeight = 100
  }

  val decimalPrecision = new IntField(2)

  val blockAgents = new IntField(1)

  def drawPolygon(g: GraphicsContext, poly: Poly, sr: Double, bounds: Rectangle2D): Unit = {

    g.fill = Color.Green
    g.stroke = Color.Lime


    g.beginPath()
    for {
      p1 <- poly.points.headOption.toSeq
      _ = g.moveTo(p1.x, p1.y)
      p <- poly.points.tail.toSeq
    } g.lineTo(p.x, p.y)
    g.closePath()

    g.fillPath()
    g.strokePath()

    g.fill = Color.Grey
    for (p <- poly.points)
      g.fillArc(p.x - sr, p.y - sr, sr * 2, sr * 2, 0, 360, ArcType.Open)

    g.fill = Color.Orange
    for {
      edges <- bpcEdges()
      (p, _) <- edges
    } g.fillArc(p.x - sr, p.y - sr, sr * 2, sr * 2, 0, 360, ArcType.Open)

    g.fill = Color(1, 1, 1, 0.5)
    g.stroke = Color(0, 0, 0, 0.5)

    val cx = (bounds.minX + bounds.maxX) / 2.0
    val cy = (bounds.minY + bounds.maxY) / 2.0

    g.strokeLine(bounds.minX, 0, bounds.maxX, 0)
    g.strokeLine(0, bounds.minY, 0, bounds.maxY)

    g.fillOval(-0.4, -0.4, 0.8, 0.8)
    g.strokeOval(-0.4, -0.4, 0.8, 0.8)
  }

  val (cw, ch) = (300.0, 300.0)
  val cm = 30.0
  val (cwo, cho) = (cw + cm * 2, ch + cm * 2)
  val cLineWidth = 2.0

  def inBounds(p: Point2D): Boolean = p.x > 0 && p.y > 0 && p.x < cwo && p.y < cho

  val bpcHandleRadius = 6.0

  val bpcPolygon = new javafx.beans.binding.ObjectBinding[Option[Poly]] {
    bind(blockTypePane.selectionModel().selectedItems)
    override def computeValue(): Option[Poly] =
      blockTypePane.selectionModel().selectedItems.headOption.filter(_.points.size > 2)
  }
  val bpcBounds = new javafx.beans.binding.ObjectBinding[Option[Rectangle2D]] {
    bind(bpcPolygon)
    override def computeValue(): Option[Rectangle2D] = for (poly <- bpcPolygon()) yield {
      val minX = poly.points.minBy(_.x).x
      val minY = poly.points.minBy(_.y).y
      val maxX = poly.points.maxBy(_.x).x
      val maxY = poly.points.maxBy(_.y).y
      new Rectangle2D(minX, minY, maxX - minX, maxY - minY)
    }
  }
  val bpcScale = new javafx.beans.binding.ObjectBinding[Option[Double]] {
    bind(bpcBounds)
    override def computeValue() = for (rect <- bpcBounds()) yield {
      math.min(cw / rect.width, ch / rect.height)
    }
  }
  val bpcEdges = new javafx.beans.binding.ObjectBinding[Option[Seq[(Vec2, Int)]]] {
    bind(bpcPolygon)
    override def computeValue() = for (poly <- bpcPolygon()) yield {
      val edges = poly.points :+ poly.points.head sliding 2
      for ((Array(p1, p2), i) <- edges.zipWithIndex.toSeq) yield {
        (p1 add p2 mul 0.5f, i)
      }
    }
  }
  val bpcExists = new javafx.beans.binding.BooleanBinding {
    bind(bpcPolygon)
    override def computeValue(): Boolean = bpcPolygon().exists(_.points.size > 2)
  }

  def paintBlockPreviewCanvas(): Unit = {
    val g = blockPreviewCanvas.graphicsContext2D
    g.clearRect(0, 0, cwo, cho)
    for {
      poly <- bpcPolygon()
      bounds <- bpcBounds()
      scale <- bpcScale()
    } {
      g.save()

      g.translate(cm, cm)
      g.scale(scale, scale)
      g.translate(-bounds.minX, -bounds.minY)
      g.lineWidth = cLineWidth / scale
      val sr = bpcHandleRadius / scale

      drawPolygon(g, poly, sr, bounds)

      g.restore()

      for {
        (p, i) <- dragStart
        to <- dragTo
      } {
        g.stroke = Color.Black
        val tx = cm + (p.x - bounds.minX) * scale
        val ty = cm + (p.y - bounds.minY) * scale
        g.strokeLine(tx, ty, to.x, to.y)
      }
    }
  }

  var dragStart: Option[(Vec2, Int)] = None
  var dragTo: Option[Point2D] = None

  def replacePoly(poly: Poly, newPoly: Poly): Unit = {
    try {
      val index = blockTypePane.items().indexOf(poly)
      blockTypePane.items().set(index, newPoly)
      blockTypePane.selectionModel().select(newPoly)
    } catch {
      case _: Throwable =>
    }
  }
  val blockPreviewCanvas = new Canvas(cw + cm * 2, ch + cm * 2) {
    onMousePressed = (e: MouseEvent) => {
      val hit = new Point2D(e.x, e.y)
      for {
        poly <- bpcPolygon()
        bounds <- bpcBounds()
        scale <- bpcScale()
        start @ (_, i) <- poly.points.zipWithIndex.find {
          case (p, _) =>
            val tx = cm + (p.x - bounds.minX) * scale
            val ty = cm + (p.y - bounds.minY) * scale
            hit.distance(tx, ty) < bpcHandleRadius
        }
      } e.button match {
        case MouseButton.PRIMARY =>
          dragStart = Some(start)
          dragTo = Some(hit)
        case MouseButton.SECONDARY if poly.points.size > 3 =>
          val first = poly.points.take(i)
          val second = poly.points.takeRight(poly.points.size - 1 - i)
          replacePoly(poly, Poly(first ++ second, poly.agents))
        case _ =>
      }
    }
    onMouseDragged = (e: MouseEvent) => {
      if(dragStart.isDefined) {
        dragTo = Some(new Point2D(e.x, e.y))
        paintBlockPreviewCanvas()
      }
    }
    onMouseReleased = (e: MouseEvent) => {
      for {
        poly <- bpcPolygon() if e.button == MouseButton.PRIMARY
        bounds <- bpcBounds()
        scale <- bpcScale()
        (p, i) <- dragStart
        to <- dragTo //if inBounds(to)
      } {
        dragStart = None
        dragTo = None
        val prec = decimalPrecision.property.value
        val x2 = trunc(bounds.minX + (to.x - cm) / scale, prec)
        val y2 = trunc(bounds.minY + (to.y - cm) / scale, prec)
        val p2 = Vec2(x2.toFloat, y2.toFloat)
        replacePoly(poly, Poly(poly.points.updated(i, p2), poly.agents))
      }
    }
    onMouseClicked = (e: MouseEvent) => {
      for {
        edges <- bpcEdges() if e.button == MouseButton.PRIMARY
        bounds <- bpcBounds()
        scale <- bpcScale()
        poly <- bpcPolygon()
        hit = new Point2D(e.x, e.y)
        (p, i) <- edges.find {
          case (p, _) =>
            val tx = cm + (p.x - bounds.minX) * scale
            val ty = cm + (p.y - bounds.minY) * scale
            hit.distance(tx, ty) < bpcHandleRadius
        }
        split = (i + 1) % edges.size
        (first, second) = poly.points.splitAt(split)
      } replacePoly(poly, Poly(first ++: p +: second, poly.agents))
    }
  }

  val removeBlockType = new Button("Remove") {
    disable <== !bpcExists

    onAction = handle {
      for {
        i <- 0 until blockTypePane.items().size()
        if blockTypePane.selectionModel().isSelected(i)
      } blockTypePane.items().remove(i)
      if(blockTypePane.items().isEmpty || blockTypePane.items().last.points.size > 0)
        blockTypePane.items() += Poly(Array(), 0)
    }
  }

  val copyBlockType = new Button("Duplicate") {
    disable <== !bpcExists

    onAction = handle {
      for {
        item <- blockTypePane.selectionModel().selectedItems.headOption
      } {
        blockTypePane.items() -= blockTypePane.items().last
        blockTypePane.items() += item.copy()
        blockTypePane.items() += Poly(Array(), 0)
      }
    }
  }
  
  minWidth <== prefWidth
  children = Seq(
    Label("Block Types:"),
    blockTypePane,
    "Decimal Precision:" @: decimalPrecision,
    "Number of Agents:" @: blockAgents,
    new HBox { children = Seq(Spacer(), blockPreviewCanvas, Spacer())},
    new HBox(5) { children = Seq(Spacer(), copyBlockType, removeBlockType, Spacer())}
  )
  blockAgents.property.onInvalidate(_ => {
    val poly = blockTypePane.selectionModel().getSelectedItem
    replacePoly(poly, poly.copy(agents = blockAgents.property.value))
  })
  blockTypePane.selectionModel().selectedItems.onInvalidate(_ => {
    for (poly <- blockTypePane.selectionModel().getSelectedItems.headOption) {
      blockAgents.text = "" + poly.agents
    }
    paintBlockPreviewCanvas()
  })
  blockTypePane.selectionModel().select(0)
}

object BlockPreview {
  def parsePolygon(points: String): Option[Poly] = {
    if (points.size > 0) {
      val parts = points.split("#")
      for {
        ag <- parts.headOption
        pts <- parts.tail.headOption
      } yield {
        val polyPts = for {
          Array(x, y) <- pts.split("[\\s,]+").grouped(2).toArray
          point <- Try(Vec2(x.toFloat, y.toFloat)).toOption
        } yield point
        Poly(polyPts, ag.toInt)
      }
    } else None
  }

  class PolyListCell extends TextFieldListCell[Poly](StringConverter[Poly](
    pts => parsePolygon(pts).getOrElse(Poly(Array[Vec2](), 1)), _.toString
  ))
}