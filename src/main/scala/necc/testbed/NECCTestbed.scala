package necc.testbed

import java.awt.event.{ActionEvent, ActionListener, WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Component}
import javax.swing.{JButton, JFrame, JScrollPane, UIManager}

import necc.gui.NECCGUI
import org.jbox2d.testbed.framework.TestbedController.UpdateBehavior
import org.jbox2d.testbed.framework._
import org.jbox2d.testbed.framework.j2d.{TestPanelJ2D, TestbedSidePanel}

import scala.util.Try
import scalafx.application.Platform

object NECCTestbed {
  try {
    UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel")
  } catch {
    case e: Exception =>
    //log.warn("Could not set the look and feel to nimbus.")
  }

  def launchTestbed(test: NECCTest): Unit = {

    val model: TestbedModel = new TestbedModel
    val panel: TestbedPanel = new TestPanelJ2D(model)
    model.addTest(test)
    val controller = new TestbedController(model, panel, UpdateBehavior.UPDATE_CALLED)
    val side = new TestbedSidePanel(model, controller)
    model.addCategory("NECC")
    val testbed: JFrame = new JFrame("NECC Visualisation") {

      setLayout(new BorderLayout)

      model.setDebugDraw(panel.getDebugDraw)

      // access private quitButton field
      for (fieldOpt <- Try(Option(classOf[TestbedSidePanel].getDeclaredField("quitButton"))); field <- fieldOpt) {
        field.setAccessible(true)
        val quitButton = field.get(side).asInstanceOf[JButton]
        quitButton.getActionListeners foreach quitButton.removeActionListener
        quitButton.addActionListener(new ActionListener {
          override def actionPerformed(e: ActionEvent): Unit = closeTest()
        })
      }

      addWindowListener(new WindowAdapter {
        override def windowClosing(e: WindowEvent): Unit = closeTest()
      })

      add(panel.asInstanceOf[Component], "Center")
      add(new JScrollPane(side), "East")
      pack()

      controller.playTest(0)
      controller.start()

      def closeTest(): Unit = {
        test.end()
        setVisible(false)
        Platform.runLater {
          NECCGUI.showVis.disable = false
        }
        controller.stop()
      }
    }

    testbed.setVisible(true)
  }
}

