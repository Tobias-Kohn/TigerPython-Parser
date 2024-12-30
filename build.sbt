import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val useScalaVersion = "2.13.14"
val releaseVersion = "1.0.4"

val sharedSettings = Seq(
  scalaVersion := useScalaVersion,
  version := releaseVersion,
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  target := {
    // From https://stackoverflow.com/questions/33184731/sbt-crossproject-build-broken-with-custom-compile-target
    /* Hacky way to detect whether this is a Scala.js project
     * without pulling the Scala.js sbt plugin on the classpath.
     */
    val isScalaJS = libraryDependencies.value.exists { dep =>
      dep.name.startsWith("scalajs-library") // not tested
    }
    file("tpParser") / (if (isScalaJS) "js" else "jvm") / "target" / name.value
  }
)

lazy val tpParser =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("tpParser"))
    .settings(sharedSettings :+ (name := "TigerPython Parser"))
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.8"
    )
    .jvmSettings()

lazy val tpParserModuleJS =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("tpParser"))
    .settings(sharedSettings :+ (name := "TigerPython Parser Module"))
    .jsSettings(
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.8",
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }
    )
    .jvmSettings()

// Disable tests in the module version because it complains about exporting:
tpParserModuleJS.js / test := {}

// Define the custom task
lazy val makeRelease = taskKey[Unit]("Run fullOptJS on both configurations and copy the output files to the release directory")

makeRelease := {
  val log = streams.value.log

  // Paths to the output files
  val scalaDir = "scala-" + useScalaVersion.split("\\.").slice(0, 2).mkString(".")
  val outputPlain = (tpParser.js / target).value / scalaDir / "tigerpython-parser-opt.js"
  val outputModule = (tpParserModuleJS.js / target).value / scalaDir / "tigerpython-parser-module-opt.js"

  // Paths to the release directory
  val releasePlain = baseDirectory.value / "release" / "tigerpython-parser.js"
  val releaseModule = baseDirectory.value / "release" / "tigerpython-parser.mjs"

  // Run fullOptJS for the configuration without ESModule
  log.info("Running fullOptJS for plain and module...")
  (tpParser.js / Compile / fullOptJS).value
  (tpParserModuleJS.js / Compile / fullOptJS).value
  log.info(s"Copying ${outputPlain} to ${releasePlain}...")
  IO.copyFile(outputPlain, releasePlain)
  log.info(s"Copying ${outputModule} to ${releaseModule}...")
  IO.copyFile(outputModule, releaseModule)

  // Regenerate the package.json and package-lock.json file:
  val packageJsonString =
    s"""
       |{
       |  "name": "tigerpython-parser",
       |  "version": "${releaseVersion}",
       |  "description": "Enhanced error recognition in Python ",
       |  "main": "release/tigerpython-parser.mjs",
       |  "types": "tpParser/js/types/index.d.ts",
       |  "directories": {
       |    "doc": "doc"
       |  },
       |  "scripts": {
       |    "test": "sbt test",
       |    "build": "sbt makeRelease"
       |  },
       |  "repository": {
       |    "type": "git",
       |    "url": "git+https://github.com/Tobias-Kohn/TigerPython-Parser.git"
       |  },
       |  "author": "Tobias Kohn",
       |  "license": "MPL-2.0",
       |  "bugs": {
       |    "url": "https://github.com/Tobias-Kohn/TigerPython-Parser/issues"
       |  },
       |  "homepage": "https://github.com/Tobias-Kohn/TigerPython-Parser#readme",
       |  "dependencies": {}
       |}
       |""".stripMargin
  val packageLockJsonString =
    s"""
       |{
       |  "name": "tigerpython-parser",
       |  "version": "${releaseVersion}",
       |  "lockfileVersion": 1
       |}
       |""".stripMargin

  log.info("Regenerating package.json and package-lock.json")
  java.nio.file.Files.writeString(new File("package.json").toPath, packageJsonString, java.nio.charset.StandardCharsets.UTF_8)
  java.nio.file.Files.writeString(new File("package-lock.json").toPath, packageLockJsonString, java.nio.charset.StandardCharsets.UTF_8)
}