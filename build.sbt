import ReleaseTransformations._

val scala212V = "2.12.11"
val scala213V = "2.13.1"
val mainScalaVersion = scala213V
val compatScalaVersion = scala212V

val catsV = "2.1.1"
val xmlsV = "0.1.10"
val circeVersion = "0.12.3"

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
    "org.scalatest" %%% "scalatest" % "3.0.8" % Test ::
    "org.scalamock" %% "scalamock" % "4.4.0" % Test ::
    "org.scalacheck" %% "scalacheck" % "1.14.1-RC2" % Test ::
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
  scalacOptions += (if (scalaVersion.value.contains("2.13")) "" else "-Ypartial-unification"),
  coverageExcludedPackages := "<empty>;"
)

lazy val circeDependencies = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

lazy val core = (crossProject in file(".") / "core")
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js.settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"
)
lazy val coreJVM = core.jvm

lazy val layout = (crossProject in file(".") / "layout")
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

lazy val graphml = (crossProject in file(".") / "graphml")
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
  ).dependsOn(core, layout, cats)

lazy val graphmlJS = graphml.js
lazy val graphmlJVM = graphml.jvm

lazy val editor = (crossProject in file(".") / "editor")
  .settings(commonSettings)
  .settings(
    name := "graphs-editor"
  ).dependsOn(core, graphml, json)

lazy val editorJS = editor.js.settings(
  scalaJSUseMainModuleInitializer := true
)
lazy val editorJVM = editor.jvm

lazy val cats = (crossProject in file(".") / "cats")
  .settings(commonSettings)
  .settings(
    name := "graphs-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV % Provided
    )
  ).dependsOn(core)

lazy val catsJS = cats.js
lazy val catsJVM = cats.jvm

lazy val json = (crossProject in file(".") / "json")
  .settings(commonSettings)
  .settings(
    name := "graphs-json",
    libraryDependencies ++= circeDependencies.map(_ % Provided)
  ).dependsOn(core)

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm

lazy val examples = (crossProject in file("examples"))
      .settings(commonSettings)
      .settings(
        name := "graphs-examples",
        libraryDependencies ++= Seq(
          "org.typelevel" %%% "cats-core" % catsV
        ) ++ circeDependencies
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
    jsonJVM
  )

addCommandAlias("testWithCoverage", ";clean;coverage;test;coverageReport")
