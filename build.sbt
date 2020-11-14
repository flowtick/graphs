import ReleaseTransformations._

val scala212V = "2.12.11"
val scala213V = "2.13.2"
val mainScalaVersion = scala213V
val compatScalaVersion = scala212V

val catsV = "2.1.1"
val xmlsV = "0.1.11"
val circeVersion = "0.13.0"

lazy val commonSettings = Seq(
  resolvers ++= Seq(
    Resolver.bintrayRepo("flowtick", "jgraphx")
  ),
  organization := "com.flowtick",
  scalaVersion := mainScalaVersion,
  crossScalaVersions := Seq(mainScalaVersion, compatScalaVersion),
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseCrossBuild := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges
  ),
  libraryDependencies ++=
    "org.scalatest" %%% "scalatest" % "3.2.2" % Test ::
    "org.scalamock" %% "scalamock" % "5.0.0" % Test ::
    "org.scalacheck" %% "scalacheck" % "1.14.1" % Test ::
    Nil,
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("https://flowtick.github.io/graphs")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/flowtick/graphs"),
      "scm:git@github.com:flowtick/graphs.git"
    )
  ),
  developers := List(
    Developer(id = "adrobisch", name = "Andreas Drobisch", email = "github@drobisch.com", url = url("http://drobisch.com"))
  ),
  autoAPIMappings := true,
  siteSubdirName in ScalaUnidoc := "latest/api",
  scalacOptions += (if (scalaVersion.value.contains("2.13")) "" else "-Ypartial-unification")
)

lazy val core = (crossProject(JVMPlatform, JSPlatform) in file(".") / "core")
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js.settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"
)
lazy val coreJVM = core.jvm

lazy val style = (crossProject(JVMPlatform, JSPlatform) in file(".") / "style")
  .settings(commonSettings)
  .settings(
    name := "graphs-style",
  ).dependsOn(core)

lazy val styleJS = style.js
lazy val styleJVM = style.jvm

lazy val layout = (crossProject(JVMPlatform, JSPlatform) in file(".") / "layout")
  .settings(commonSettings)
  .settings(
    name := "graphs-layout",
  ).jvmSettings(
  libraryDependencies ++= Seq(
    "com.mxgraph" % "jgraphx" % "3.7.4"
  )
).dependsOn(core)

lazy val layoutJS = layout.js
lazy val layoutJVM = layout.jvm

lazy val graphml = (crossProject(JVMPlatform, JSPlatform) in file(".") / "graphml")
  .settings(commonSettings)
  .settings(
    name := "graphs-graphml",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV % Provided
    )
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "com.flowtick" %% "xmls" % xmlsV
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "com.flowtick" %%% "xmls" % xmlsV
    )
  ).dependsOn(core, style, layout, cats)

lazy val graphmlJS = graphml.js
lazy val graphmlJVM = graphml.jvm

lazy val editor = (crossProject(JVMPlatform, JSPlatform) in file(".") / "editor")
  .settings(commonSettings)
  .settings(
    name := "graphs-editor",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "2.1.3",
    libraryDependencies += "org.apache.xmlgraphics" % "batik-rasterizer" % "1.13"
  ).dependsOn(core, graphml, json, cats)

lazy val editorJS = editor.js.settings(
  scalaJSUseMainModuleInitializer := true,
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1"
)

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val editorJVM = editor.jvm.settings(
  libraryDependencies += "org.scalafx" %% "scalafx" % "14-R19",
  libraryDependencies += "org.fxmisc.richtext" % "richtextfx" % "0.10.5",
  libraryDependencies ++= javaFXModules.map( m =>
    "org.openjfx" % s"javafx-$m" % "14.0.1" classifier osName
  )
)

lazy val cats = (crossProject(JVMPlatform, JSPlatform) in file(".") / "cats")
  .settings(commonSettings)
  .settings(
    name := "graphs-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV % Provided
    )
  ).dependsOn(core)

lazy val catsJS = cats.js
lazy val catsJVM = cats.jvm

lazy val json = (crossProject(JVMPlatform, JSPlatform) in file(".") / "json")
  .settings(commonSettings)
  .settings(
    name := "graphs-json",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion % Provided)
  ).dependsOn(core)

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm

lazy val examples = (crossProject(JVMPlatform, JSPlatform) in file("examples"))
      .settings(commonSettings)
      .settings(
        name := "graphs-examples",
        libraryDependencies ++= Seq(
          "org.typelevel" %%% "cats-core" % catsV
        ),
        libraryDependencies ++= Seq(
          "io.circe" %%% "circe-core",
          "io.circe" %%% "circe-generic",
          "io.circe" %%% "circe-parser"
        ).map(_ % circeVersion)
      ).jsSettings(scalaJSUseMainModuleInitializer := false)
      .dependsOn(core, graphml, cats, layout, json)

lazy val examplesJS = examples.js
lazy val examplesJVM = examples.jvm

lazy val graphs = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(commonSettings)
  .settings(
    publishLocal := {},
    publish := {},
    test := {},
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(jsonJS, graphmlJS, coreJS, catsJS, layoutJS, editorJS, examplesJS)
  ).aggregate(
    coreJS,
    coreJVM,
    examplesJS,
    examplesJVM,
    graphmlJS,
    graphmlJVM,
    catsJVM,
    catsJS,
    layoutJS,
    layoutJVM,
    editorJS,
    editorJVM,
    jsonJS,
    jsonJVM,
    styleJS,
    styleJVM
  )
