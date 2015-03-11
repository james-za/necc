import sbt.Keys._
import sbt._
import sbtassembly.Plugin
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin.{MergeStrategy, PathList}

object BuildSettings {
  def launch4j(l4jc: File, conf: File): String = ("\"" + l4jc.getPath + "\" \"" + conf.getPath + "\"").!!

  lazy val launch4jc = settingKey[File]("Launch4jc.exe location")
  lazy val exeFileDir = settingKey[File]("launch4j exe output dir")
  lazy val exeFileLocation = taskKey[File]("launch4j exe output file")
  lazy val zipResourcesDir = settingKey[File]("files included in assembled zip")
  lazy val assembledZip = settingKey[File]("location of assembled zip")
  lazy val l4jConfigLocation = settingKey[File]("launch4j config file location")
  lazy val l4jConfig = taskKey[File]("write launch4j config")
  lazy val exeFile = taskKey[File]("result of running launch4j on assembly jar")
  lazy val assembleZip = taskKey[Set[File]]("Creates a distributable zip file containing the jar, libraries and properties.")

  val assembleZipSettings = Seq(
    zipResourcesDir := baseDirectory.value / "zipresources",
    assembledZip := target.value / (name.value + "-" + version.value + ".zip"),
    assembleZip := {
      def entries(f: File): List[File] = f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries) else Nil)
      val binary = exeFile.value
      val zipFile = assembledZip.value
      val zrDir = zipResourcesDir.value
      val zipFunc = (zrFiles: Set[File]) => {
        val zrPairs = (zrFiles - binary) pair relativeTo(zrDir)
        IO.zip((binary, binary.name) +: zrPairs, zipFile)
        Set(zipFile)
      }
      val cacheFunc = FileFunction.cached(target.value / "assemblezip-cache", FilesInfo.lastModified, FilesInfo.exists)(zipFunc)
      val zrFiles = (binary :: entries(zrDir)).toSet - zrDir
      streams.value.log.info(zrFiles.toString())
      cacheFunc(zrFiles)
    }
  )

  val launch4jSettings = Seq(
    exeFileDir := target.value / "launch4j",
    exeFileLocation := {
      IO.createDirectory(exeFileDir.value)
      exeFileDir.value / (name.value + ".exe")
    },
    exeFile := {
      println(s"exeFile ${launch4jc.value} ${l4jConfig.value}")
      println(launch4j(launch4jc.value, l4jConfig.value))
      exeFileLocation.value
    },
    l4jConfigLocation := target.value / (name.value + "-launch4j-config.xml"),
    l4jConfig := {
      val configFile = l4jConfigLocation.value
      val jarPath = assembly.value.getPath
      val exePath = exeFileLocation.value.getPath
      val configContents = s"""
        |<launch4jConfig>
        |  <dontWrapJar>false</dontWrapJar>
        |  <headerType>gui</headerType>
        |  <jar>$jarPath</jar>
        |  <outfile>$exePath</outfile>
        |  <errTitle></errTitle>
        |  <cmdLine></cmdLine>
        |  <chdir>.</chdir>
        |  <priority>normal</priority>
        |  <downloadUrl>http://java.com/download</downloadUrl>
        |  <supportUrl></supportUrl>
        |  <stayAlive>false</stayAlive>
        |  <manifest></manifest>
        |  <icon></icon>
        |  <classPath>
        |    <mainClass>necc.gui.NECCGUI</mainClass>
        |    <cp>%JREHOMEDIR%/lib/ext/jfxrt.jar</cp>
        |    <cp>%JREHOMEDIR%/lib/jfxrt.jar</cp>
        |  </classPath>
        |  <jre>
        |    <path></path>
        |    <bundledJre64Bit>false</bundledJre64Bit>
        |    <minVersion>1.7.0_09</minVersion>
        |    <maxVersion>1.7.0_99</maxVersion>
        |    <jdkPreference>preferJre</jdkPreference>
        |    <runtimeBits>64/32</runtimeBits>
        |  </jre>
        |</launch4jConfig>
        |""".stripMargin
      IO.write(configFile, configContents)
      configFile
    }
  )

  lazy val paradiseVersion = "2.0.1"
  lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "za.jwatson",
    version := "1.4.3",
    scalacOptions ++= Seq("-deprecation", "-optimise"),
    scalaVersion := "2.11.4",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "org.jbox2d" % "jbox2d-library" % "2.2.1.1",
      "org.jbox2d" % "jbox2d-testbed" % "2.2.1.1",
      "org.scala-tools.sbinary" %% "sbinary" % "0.4.3-SNAPSHOT" cross CrossVersion.full,
      "org.scalafx" %% "scalafx" % "2.2.67-R10",
      "com.typesafe.akka" %% "akka-actor" % "2.3.6",
      "com.github.nscala-time" %% "nscala-time" % "1.4.0",
      "org.jfxtras" % "jfxtras-labs" % "2.2-r5",
      "com.github.tototoshi" %% "scala-csv" % "1.1.2"
    ),
    // dependencies of ahni
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.1.1",
      "jakarta-regexp" % "jakarta-regexp" % "1.4",
      "com.beust" % "jcommander" % "1.35",
      "log4j" % "log4j" % "1.2.17",
      "org.apache.commons" % "commons-lang3" % "3.1"
    ),
    javacOptions ++= Seq(
      "-target", "1.7",
      "-source", "1.7",
      "-Xlint:deprecation"),
    fork := true,
    unmanagedResourceDirectories in Compile += baseDirectory.value / "zipresources"
  )
}

object NECCBuild extends Build {
  import BuildSettings._

  val MergeIgnoringDuplicateLog4j: PartialFunction[String, MergeStrategy] = {
    case PathList("org", "apache", "log4j", _*) => MergeStrategy.first
    case PathList("META-INF", "maven", "log4j", "log4j", _*) => MergeStrategy.first
  }

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Plugin.assemblySettings ++ Seq(
      name := "NECC",
      mainClass := Some("necc.NECC"),
      mainClass in assembly := Some("necc.NECC"),
      jarName in assembly := "NECC.jar",
      unmanagedResourceDirectories in Compile += baseDirectory.value / "zipresources",
//      mergeStrategy in assembly ~= ((old: String => MergeStrategy) => {
//        case PathList("org", "apache", "log4j", _*) => MergeStrategy.first
//        case PathList("META-INF", "maven", "log4j", "log4j", _*) => MergeStrategy.first
//        case x => old(x)
//      }),
      launch4jc := file("""C:\Program Files (x86)\Launch4j\launch4jc.exe""")
    ) ++ launch4jSettings ++ assembleZipSettings
  ).dependsOn(macros)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      name := "NECC macros",
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
    )
  )

}