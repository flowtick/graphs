import ReleaseTransformations._

val mainScalaVersion = "2.12.8"
val scalaXmlV = "1.1.1"
val catsV = "1.5.0"

lazy val commonSettings = Seq(
  resolvers ++= Seq(
    Resolver.bintrayRepo("flowtick", "jgraphx")
  ),
  organization := "com.flowtick",
  scalaVersion := mainScalaVersion,
  crossScalaVersions := Seq(mainScalaVersion, "2.11.12"),
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
    "org.scalatest" %%% "scalatest" % "3.0.4" % Test ::
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test ::
    "org.scalacheck" %% "scalacheck" % "1.14.0" % Test ::
    Nil,
  wartremoverErrors ++= Warts.unsafe.filterNot(Seq(
    Wart.NonUnitStatements,
    Wart.DefaultArguments,
    Wart.Any
  ).contains(_)),
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  publishMavenStyle := true,
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
  scalacOptions += "-Ypartial-unification",
  coverageExcludedPackages := "<empty>;"
)

lazy val core = (crossProject in file(".") / "core")
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js.settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
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
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlV
    )
  ).jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-xml" % scalaXmlV
    )
  ).dependsOn(core, layout)

lazy val graphmlJS = graphml.js
lazy val graphmlJVM = graphml.jvm

lazy val editor = (crossProject in file(".") / "editor")
  .settings(commonSettings)
  .settings(
    name := "graphs-editor",
  ).dependsOn(core, graphml)

lazy val editorJS = editor.js.settings(
  scalaJSUseMainModuleInitializer := true
)
lazy val editorJVM = editor.jvm

lazy val cats = (crossProject in file(".") / "cats")
  .settings(commonSettings)
  .settings(
    name := "graphs-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsV
    )
  ).dependsOn(core)

lazy val catsJS = cats.js
lazy val catsJVM = cats.jvm

lazy val examples = (project in file("examples"))
      .settings(commonSettings)
      .settings(
        name := "graphs-examples"
      )
      .dependsOn(coreJVM, graphmlJVM, catsJVM, layoutJVM)

lazy val graphs = (project in file("."))
  .enablePlugins(ParadoxSitePlugin, ScalaUnidocPlugin, GhpagesPlugin)
  .settings(commonSettings)
  .settings(
    publishLocal := {},
    publish := {},
    test := {},
    sourceDirectory in Paradox := baseDirectory.value / "docs",
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    paradoxProperties += ("version" -> version.value),
    mappings in makeSite ++= Seq(
      file("LICENSE") -> "LICENSE"
    ),
    git.remoteRepo := "git@github.com:flowtick/graphs.git",
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(graphmlJS, coreJS, catsJS, layoutJS, editorJS),
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
  ).aggregate(
    coreJS,
    coreJVM,
    examples,
    graphmlJS,
    graphmlJVM,
    catsJVM,
    catsJS,
    layoutJS,
    layoutJVM,
    editorJS,
    editorJVM
  )

addCommandAlias("testWithCoverage", ";clean;coverage;test;coverageReport")
