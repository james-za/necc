package necc.gui

import java.io.File
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import javafx.geometry

import akka.util.Timeout
import com.anji.neat.NeatConfiguration
import com.github.nscala_time.time.Imports._
import com.ojcoleman.ahni.evaluation.BulkFitnessFunctionMT
import com.ojcoleman.ahni.hyperneat.{HyperNEATEvolver, HyperNEATConfiguration, Properties}
import necc.NECCRunner
import necc.ann.{ANNContext, BainNNContext, TestANNContext}
import necc.experiment.Experiment
import necc.experiment.Experiment.{CompleteTask, ExperimentTask, LoadBlocksTask, PlaceBlocksTask}
import necc.hyperneat.NECCFitnessFunction
import necc.hyperneat.NECCFitnessFunction.{Reset, Best, Evaluation}
import necc.task.TaskSettings
import necc.testbed.{NECCTest, NECCTestbed}
import org.jgapcustomised.ChromosomeMaterial
import sbinary.Operations

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.Try
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.collections.ObservableBuffer
import scalafx.geometry._
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Stage

object NECCGUI extends JFXApp {
  def outputPath(fitness: Double, run: Int) = {
    val fn = NECCGUI.outputName.text.value
    val f = (fitness * 100.0).toInt
    val t = NECCGUI.taskCombo.getSelectionModel.getSelectedItem.name
    val name = fn
      .replaceAll("""\$F""", "" + f)
      .replaceAll("""\$T""", "" + t)
      .replaceAll("""\$R""", "" + run)
    val rd = new File("rundata")
    rd.mkdir()
    rd.toPath.resolve(name)
  }

  lazy val threads = IntField2(0)

  lazy val runs = IntField2(1)
  lazy val generations = IntField2(500)
  lazy val population = IntField2(100)
  lazy val repetitions = IntField2(3)

  lazy val maxTime = new FloatField(120.0f)
  lazy val stepFrequency = new FloatField(20.0f)
  lazy val intervalController = new FloatField(1.0f)

  lazy val sensors = new IntField(8) {
    property.onInvalidate(paintSensorCanvas())
  }
  lazy val range = new DoubleField(50) {
    property.onInvalidate(paintSensorCanvas())
  }

  lazy val sensorCanvas = new SensorCanvas

  def paintSensorCanvas(): Unit = {
    sensorCanvas.paint(
      n = sensors.property.value,
      range = range.property.value,
      rAgent = 40, rSensor = 140)
  }

  val experiments = Map[ExperimentTask, String](
    Experiment.LoadBlocksTask -> "ahni-load-radial.properties",
    Experiment.PlaceBlocksTask -> "ahni-place-radial.properties"
  )
  val fileNames = (for {
    (t, e) <- experiments
    fileName <- Seq(e, "zipresources/" + e).find(new File(_).exists())
  } yield t -> fileName).toMap[ExperimentTask, String]

  lazy val taskCombo = new ComboBox[ExperimentTask](Seq(LoadBlocksTask, PlaceBlocksTask, CompleteTask)) {
    selectionModel().selectedItemProperty().onChange((_, _, newVal) => newVal match {
      case CompleteTask => startRun.disable = true; resumeRun.disable = true
      case _ => startRun.disable = false; resumeRun.disable = false
    })
    selectionModel().select(2)
  }

  lazy val outputName = new TextField() {
    hgrow = Priority.Always
  }

  lazy val startRun = new Button("Run Experiment") {
    onAction = handle { runExperiment() }
  }

  lazy val resumeRun = new Button("Resume Run") {
    onAction = handle { runExperiment(resume = true) }
  }

  lazy val chart = new LineChart(NumberAxis("Generation"), NumberAxis("Fitness")) {
    prefWidth = 800
    prefHeight = 600
  }

  val cachedRunData = collection.mutable.Map[Path, RunData]()

  def updateChart(runList: ListView[Path]): Unit = {
    import necc.gui.RunDataProtocol._
    val series = for {
      (path, i) <- runList.selectionModel().selectedItems.zipWithIndex
      runData = cachedRunData.getOrElseUpdate(path, Operations.fromFile[RunData](path.toFile))
      Run(run, runGens) <- runData.runs.headOption
      dataSeq = for (Gen(gen, fit) <- runGens) yield
        XYChart.Data[Number, Number](gen, fit)
    } yield XYChart.Series[Number, Number](s"$i: ${path.getFileName.toString}", ObservableBuffer(dataSeq.toSeq))
    Platform.runLater {
      chart.data = series
    }
  }

  def loadRuns(selected: Option[Path]): Unit = {
    Platform.runLater {
      runListLoad.items().clear()
      runListPlace.items().clear()
    }
    import necc.gui.RunDataProtocol._
    for {
      rds <- Option(new File("rundata").listFiles()).toSeq
      file <- rds if file.getName endsWith ".sbin"
      path = file.toPath
      rd <- Try(cachedRunData.getOrElseUpdate(path, Operations.fromFile[RunData](file)))
      runList <- rd.task match {
        case LoadBlocksTask => Some(runListLoad)
        case PlaceBlocksTask => Some(runListPlace)
        case CompleteTask => None
      }
    } Platform.runLater {
      runList.items().add(path)
      if (selected contains path) runList.selectionModel().select(path)
    }
  }

  def updateEvalProps(props: Properties): Unit = {
    props.setProperty(BulkFitnessFunctionMT.MAX_THREADS_KEY, threads.getNumber.intValue().toString)
    props.setProperty(HyperNEATConfiguration.NUM_RUNS_KEY, runs.getNumber.intValue().toString)
    props.setProperty(HyperNEATEvolver.NUM_GENERATIONS_KEY, generations.getNumber.intValue.toString)
    props.setProperty(NeatConfiguration.POPUL_SIZE_KEY, population.getNumber.intValue.toString)
    props.setProperty("evaluate.duration.maximum", "" + maxTime.property.value)
    props.setProperty("evaluate.interval.step", "" + 1.0 / stepFrequency.property.value)
    props.setProperty("evaluate.interval.controller", "" + intervalController.property.value)
    props.setProperty("evaluate.blocks.types", blockPane.blockTypePane.items().mkString("|"))
    props.setProperty("evaluate.sensors.count", "" + sensors.property.value)
    props.setProperty("evaluate.sensors.range", "" + range.property.value)
    props.setProperty("evaluate.repetitions", "" + repetitions.getNumber.intValue.toString)
  }

  def createProps(fileName: String) = {
    val props = new Properties(fileName)
    updateEvalProps(props)
    props
  }

  def createProps(): Properties = {
    val props = new Properties()
    updateEvalProps(props)
    props
  }

  def runExperiment(resume: Boolean = false): Unit = {
    Platform.runLater {
      progress.progress = 0
      progress2.progress = 0
      startRun.disable = true
      resumeRun.disable = true
      taskCombo.disable = true
      progress.disable = false
      progress2.disable = false
    }
    val task = taskCombo.getSelectionModel.getSelectedItem
    val fileName = fileNames(task)
    val props = createProps(fileName)

    val material = for {
      runList <- task match {
        case LoadBlocksTask => Some(runListLoad)
        case PlaceBlocksTask => Some(runListPlace)
        case CompleteTask => None
      }
      if resume
      path <- runList.selectionModel().getSelectedItems.headOption
      rd <- cachedRunData.get(path)
      (mat, _) <- rd.anns.headOption
    } yield mat

    Future { NECCRunner.run(props, material) }
  }

  lazy val progress = new ProgressBar {
    disable = true
    progress = 0
  }
  lazy val progress2 = new ProgressBar {
    disable = true
    progress = 0
  }

  def getANNContext(listView: ListView[Path]): Option[ANNContext] = for {
    path <- Option(listView.selectionModel().getSelectedItem)
    runData <- cachedRunData.get(path)
    (_, ann) <- runData.anns.headOption
  } yield new BainNNContext(ann)

  lazy val showVis = new Button("Show Visualisation") {
    onAction = handle {
      for {
        task <- Option(taskCombo.selectionModel().getSelectedItem)
        load <- getANNContext(runListLoad) orElse Some(new TestANNContext)
        place <- getANNContext(runListPlace) orElse Some(new TestANNContext)
      } {
        val props = createProps()
        val settings = TaskSettings.fromProps(props)
        NECCTestbed.launchTestbed(new NECCTest(settings, task, load, place))
        disable = true
      }
    }
  }

  def HSeparator = new Separator(new javafx.scene.control.Separator(geometry.Orientation.HORIZONTAL))
  def VSeparator = new Separator(new javafx.scene.control.Separator(geometry.Orientation.VERTICAL))
  
  lazy val sensorPane = new VBox(5) {
    content = Seq(
      "Sensors:" @: sensors,
      "Range:" @: range,
      new HBox { content = Seq(Spacer(), sensorCanvas, Spacer()) }
    )
  }

  lazy val blockPane = new BlockPreview
  
  lazy val runPane = new VBox(5) {
    minWidth <== prefWidth
    margin = Insets(5)
    content = Seq(
      new HBox(5) { content = Seq(Label("Task:"), taskCombo, Spacer(), showVis) },
      HSeparator,
      "Threads (set to number of CPU cores if equal to 0):" @: threads,
      HSeparator,
      "Runs:" @: runs,
      "Generations:" @: generations,
      "Population:" @: population,
      "Repetitions:" @: repetitions,
      HSeparator,
      "Maximum Simulation Time (seconds):" @: maxTime,
      "Controller Interval (seconds):" @: intervalController,
      "Physics Step Frequency (Hz):" @: stepFrequency,
      HSeparator,
      sensorPane,
      HSeparator,
      Label("Output run data filename:"),
      Label("\t$T is replaced with current task"),
      Label("\t$F is replaced with best fitness"),
      outputName,
      startRun :@ "Start neuro-evolution for the selected task",
      resumeRun :@ "Use the selected result as the starting point for a new run"
    )

    outputName.text = s"${DateTime.now.getMillis.toHexString}_$$T_$$R_$$F.sbin"
    //scalafx.beans.value.ObservableValue
    //new TextField().text <== when (generations > 0) choose (StringProperty("") + generations) otherwise (("": ObservableStringValue) + generations)
  }

  lazy val runListLoad = new scalafx.scene.control.ListView[Path] {
    hgrow = Priority.Always
    maxWidth = 3000
    items = ObservableBuffer()
    selectionModel().selectedItems.onInvalidate {
      updateChart(this)
    }
    selectionModel().selectionMode = SelectionMode.MULTIPLE
  }

  lazy val runListPlace = new scalafx.scene.control.ListView[Path] {
    hgrow = Priority.Always
    maxWidth = 3000
    items = ObservableBuffer()
    selectionModel().selectedItems.onInvalidate {
      updateChart(this)
    }
    selectionModel().selectionMode = SelectionMode.MULTIPLE
  }

  lazy val rlpLoad = new VBox(5) {
    hgrow = Priority.Always

    maxWidth = Double.MaxValue
    content = Seq(
      Label("Load Blocks Results"),
      runListLoad
    )
  }
  lazy val rlpPlace = new VBox(5) {
    maxWidth = Double.MaxValue
    hgrow = Priority.Always
    content = Seq(
      Label("Place Blocks Results"),
      runListPlace
    )
  }

  stage = new PrimaryStage {
    onCloseRequest = handle {
      java.lang.System.exit(0)
    }
    scene = new Scene {
      root = new BorderPane {
        center = new HBox {
          content = Seq(
            runPane,
            blockPane,
            new VBox(5) {
              margin = Insets(5)
              hgrow = Priority.Always
              maxWidth = Double.MaxValue
              content = Seq(
                new HBox {
                  content = Seq(
                    Spacer(),
                    new Button("Show Run Data") {
                      onAction = handle {
                        graphStage.toFront()
                        graphStage.show()
                      }
                    },
                    Spacer(),
                    new Button("Refresh") {
                      onAction = handle {
                        loadRuns(None)
                      }
                    },
                    Spacer()
                  )
                },
                rlpLoad,
                rlpPlace
              )
            }
          )
        }
        bottom = new HBox {
          content = Seq(progress2, progress)
        }
        progress2.prefWidth <== this.width * 0.2
        progress.prefWidth <== this.width * 0.8
      }
    }
  }

  lazy val graphStage = new Stage() {
    scene = new Scene {
      root = chart
    }
    chart.hgrow = Priority.Always
  }

  paintSensorCanvas()
  blockPane.paintBlockPreviewCanvas()
  loadRuns(None)
  graphStage.show()
  graphStage.toFront()
}