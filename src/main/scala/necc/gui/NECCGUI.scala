package necc.gui

import java.io.{ByteArrayInputStream, File}
import java.nio.file.{Files, Paths, Path}
import javafx.geometry
import javax.xml.parsers.{DocumentBuilderFactory, DocumentBuilder}

import com.github.tototoshi.csv.CSVWriter
import necc.ChromosomeOps
import com.anji.integration.{Activator, ActivatorTranscriber}
import com.anji.neat.NeatConfiguration
import com.anji.persistence.FilePersistence
import com.github.nscala_time.time.Imports._
import com.ojcoleman.ahni.evaluation.BulkFitnessFunctionMT
import com.ojcoleman.ahni.hyperneat.{HyperNEATConfiguration, HyperNEATEvolver, Properties}
import com.ojcoleman.ahni.transcriber.{HyperNEATTranscriberBain, HyperNEATTranscriber}
import com.ojcoleman.ahni.util.PropertiesConverter
import necc.hyperneat.{NECCFitnessFunction, NECCTranscriber, GatheringFF}
import necc.{Morphology, NECCRunner}
import necc.NECCRunner.NamedDoubleAllele
import necc.ann.{ANNContext, ActivatorContext, TestANNContext}
import necc.controller.Controller
import necc.experiment.Task
import necc.experiment.Task.{Complete, Construction, Gathering}
import necc.task.TaskSettings
import necc.testbed.{NECCTest, NECCTestbed}
import org.jgapcustomised.Chromosome
import org.w3c.dom.Document
import resource.Using
import sbinary.Operations
import scala.collection.JavaConverters

import scala.util.Try
import scalafx.Includes._
import scalafx.application.JFXApp.PrimaryStage
import scalafx.application.{JFXApp, Platform}
import scalafx.beans.property.BooleanProperty
import scalafx.collections.ObservableBuffer
import scalafx.geometry._
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Stage

object NECCGUI extends JFXApp {
  def outputPath(fitness: Double, run: Int, ext: String = ".sbin") = {
    val fn = NECCGUI.outputName.text.value
    val f = (fitness * 100.0).toInt
    val t = NECCGUI.taskCombo.getSelectionModel.getSelectedItem.name
    val name = fn
      .replaceAll("""\$F""", "" + f)
      .replaceAll("""\$T""", "" + t)
      .replaceAll("""\$R""", "" + run)
    val rd = new File("rundata")
    rd.mkdir()
    rd.toPath.resolve(if (name.endsWith(ext)) name else name + ext)
  }

  lazy val threads = IntField2(0)

  lazy val runs = IntField2(1)
  lazy val generations = IntField2(100)
  lazy val population = IntField2(100)
  lazy val repetitions = IntField2(3)

  lazy val maxTime = new FloatField(TaskSettings.defaults.maxTime)
  lazy val intervalStep = new FloatField(TaskSettings.defaults.intervalStep)
  lazy val intervalController = new FloatField(TaskSettings.defaults.intervalRunANN)

  lazy val adaptiveMorphologyRate = new FloatField(1.0f)

  lazy val sensors = new IntField(5) {
    property.onInvalidate(paintSensorCanvas())
  }
  lazy val range = new DoubleField(50.0) {
    property.onInvalidate(paintSensorCanvas())
  }

  lazy val sensorCanvas = new SensorCanvas

  def paintSensorCanvas(): Unit = {
    sensorCanvas.paint(
      n = sensors.property.value,
      range = range.property.value,
      rAgent = 40, rSensor = 140)
  }

  val experiments = Map[Task, String](
    Task.Gathering -> "ahni-gathering.properties",
    Task.Construction -> "ahni-construction.properties"
  )
  val fileNames = (for {
    (t, e) <- experiments
    fileName <- Seq(e, "zipresources/" + e).find(new File(_).exists())
  } yield t -> fileName).toMap[Task, String]

  lazy val tasks = Seq(Gathering, Construction, Complete)
  lazy val taskInitial = Task.Gathering
  lazy val taskRunnable = BooleanProperty(isTaskRunnable(taskInitial))
  lazy val taskCombo = new ComboBox[Task](tasks) {
    selectionModel().selectedItemProperty().onChange((_, _, newVal) => taskRunnable.value = isTaskRunnable(newVal))
    selectionModel().select(tasks.indexOf(taskInitial))
    disable <== runService.running
  }
  def isTaskRunnable(task: Task): Boolean = task match {
    case Complete => false
    case _ => true
  }

  lazy val outputName = new TextField() {
    hgrow = Priority.Always
  }

  lazy val startRun = new Button("Run Experiment") {
    onAction = handle { runExperiment() }
    disable <== runService.running || !taskRunnable
  }

  lazy val resumeRun = new Button("Resume Run") {
    onAction = handle { runExperiment(resume = true) }
    disable <== runService.running || !taskRunnable
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
      rd <- Try(Operations.fromFile[RunData](path.toFile)).toOption
      runData = cachedRunData.getOrElseUpdate(path, rd)
      cppn <- runData.cppns.headOption
      dataSeq = for ((fit, gen) <- runData.data.zipWithIndex) yield
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
        case Gathering => Some(runListLoad)
        case Construction => Some(runListPlace)
        case Complete => None
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
    props.setProperty("evaluate.interval.step", "" + intervalStep.property.value)
    props.setProperty("evaluate.interval.controller", "" + intervalController.property.value)
    props.setProperty("evaluate.blocks.types", blockPane.blockTypePane.items().mkString("|"))
    props.setProperty("evaluate.sensors.count", "" + sensors.property.value)
    props.setProperty("evaluate.sensors.range", "" + range.property.value)
    props.setProperty("evaluate.repetitions", "" + repetitions.getNumber.intValue.toString)
    props.setProperty(Morphology.TopologicalConnectivityKey, "" + topologicalConnectivity.selected.value)
    props.setProperty(Morphology.AdaptiveKey, "" + adaptiveMorphology.selected.value)
    props.setProperty(Morphology.AdaptiveRateKey, "" + adaptiveMorphologyRate.property.value)
  }

  def createProps(fileName: String) = {
    val props = new PropertiesConverter().convert(fileName)
    updateEvalProps(props)
    props
  }

  def createProps(): Properties = {
    val props = new PropertiesConverter().convert("")
    updateEvalProps(props)
    props
  }

  lazy val runService = scalafx.concurrent.Service(scalafx.concurrent.Task(task()))

  def task(): Unit = {
    Platform.runLater {
      progress.progress = 0
      progress2.progress = 0
    }
    val task = taskCombo.getSelectionModel.getSelectedItem
    val fileName = fileNames(task)
    val props = createProps(fileName)

    //    val material = for {
    //      runList <- task match {
    //        case Gathering => Some(runListLoad)
    //        case Construction => Some(runListPlace)
    //        case Complete => None
    //      }
    //      if resume
    //      path <- runList.selectionModel().getSelectedItems.headOption
    //      rd <- cachedRunData.get(path)
    //      (mat, _) <- rd.cppns.headOption
    //    } yield mat

    NECCRunner.run(props, None)
  }

  def runExperiment(resume: Boolean = false): Unit = {
    runService.reset()
    if (runService.state() == javafx.concurrent.Worker.State.READY) {
      runService.start()
    }
  }

  lazy val progress = new ProgressBar {
    disable <== !runService.running
    progress = 0
  }
  lazy val progress2 = new ProgressBar {
    disable <== !runService.running
    progress = 0
  }

  def getANNContext(listView: ListView[Path]): Option[ANNContext] = for {
    path <- Option(listView.selectionModel().getSelectedItem)
    runData <- cachedRunData.get(path)
    cppn <- runData.cppns.headOption
  } yield {
    val task = taskCombo.getSelectionModel.getSelectedItem
    val fileName = fileNames(task)
    val props = createProps(fileName)
    val config = new HyperNEATConfiguration(props)
    val chromosome = FilePersistence.chromosomeFromXml(config, cppn)
    val transcriber = props.singletonObjectProperty(ActivatorTranscriber.TRANSCRIBER_KEY).asInstanceOf[HyperNEATTranscriber[_ <: Activator]]
    val substrate = transcriber.transcribe(chromosome)
    val sensorRange = chromosome.sensorRange.fold(50.0)(_.value)
    new ActivatorContext(substrate, sensorRange)
  }

  lazy val showVis = new Button("Show Visualisation") {
    onAction = handle {
      val props = createProps("ahni-gathering.properties")
      val settings = TaskSettings.fromProps(props)
      for {
        task <- Option(taskCombo.selectionModel().getSelectedItem)
        load <- getANNContext(runListLoad) orElse Some(new TestANNContext(sensors.property.value, 2))
        place <- getANNContext(runListPlace) orElse Some(new TestANNContext(sensors.property.value, 2))
      } {
        val controller = task match {
          case Gathering => new Controller.Gathering(load)
          case Construction => new Controller.Construction(load)
          case Complete => new Controller.Complete(load, place)
        }
        NECCTestbed.launchTestbed(new NECCTest(settings, task, controller))
        disable = true
      }
    }
  }

  def HSeparator = new Separator(new javafx.scene.control.Separator(geometry.Orientation.HORIZONTAL))
  def VSeparator = new Separator(new javafx.scene.control.Separator(geometry.Orientation.VERTICAL))
  
  lazy val sensorPane = new VBox(5) {
    children = Seq(
      "Sensors:" @: sensors,
      "Range:" @: range,
      new HBox { children = Seq(Spacer(), sensorCanvas, Spacer()) }
    )
  }

  lazy val blockPane = new BlockPreview

  lazy val topologicalConnectivity = new CheckBox("Topological Connectivity")
  lazy val adaptiveMorphology = new CheckBox("Adaptive Morphology")

  lazy val cancelRun = new Button("Cancel Run") {
    onMouseClicked = handle { runService.cancel }
    disable <== !runService.running
  }
  
  lazy val runPane = new VBox(5) {
    minWidth <== prefWidth
    margin = Insets(5)
    children = Seq(
      new HBox(5) { children = Seq(Label("Task:"), taskCombo, Spacer(), showVis) },
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
      "Physics Step Interval (seconds):" @: intervalStep,
      HSeparator,
      topologicalConnectivity,
      adaptiveMorphology,
      "Adaptive Morphology Mutation Rate:" @: adaptiveMorphologyRate,
      HSeparator,
      sensorPane,
      HSeparator,
      Label("Output run data filename:"),
      Label("\t$T is replaced with current task"),
      Label("\t$F is replaced with best fitness"),
      outputName,
      startRun :@ "Start neuro-evolution for the selected task",
      resumeRun :@ "Use the selected result as the starting point for a new run",
      cancelRun
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
    children = Seq(
      Label("Load Blocks Results"),
      runListLoad
    )
  }
  lazy val rlpPlace = new VBox(5) {
    maxWidth = Double.MaxValue
    hgrow = Priority.Always
    children = Seq(
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
          children = Seq(
            runPane,
            blockPane,
            new VBox(5) {
              margin = Insets(5)
              hgrow = Priority.Always
              maxWidth = Double.MaxValue
              children = Seq(
                new HBox {
                  children = Seq(
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
          children = Seq(progress2, progress)
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