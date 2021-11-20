val scala212V = "2.12.14"
val scala213V = "2.13.6"
val mainScalaVersion = scala213V
val compatScalaVersion = scala212V

val catsV = "2.6.1"
val xmlsV = "0.1.11"
val circeVersion = "0.14.1"

inThisBuild(List(
  organization := "com.flowtick",
  homepage := Some(url("https://flowtick.github.io/graphs")),
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(id = "adrobisch", name = "Andreas Drobisch", email = "github@drobisch.com", url = url("http://drobisch.com"))
  ),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/flowtick/graphs"),
      "scm:git@github.com:flowtick/graphs.git"
    )
  ),
  scalaVersion := mainScalaVersion,
  crossScalaVersions := Seq(mainScalaVersion, compatScalaVersion),
  githubWorkflowPublish := Seq(
    WorkflowStep.Sbt(
      List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    )
  ),
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
  githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("scalafmtCheckAll", "test", "docs/makeSite"))),
  githubWorkflowJavaVersions := Seq("adopt@1.15.0-2"),
  dynverSeparator := "-"
))

lazy val commonSettings = Seq(
  scalacOptions += (if (scalaVersion.value.contains("2.13")) "" else "-Ypartial-unification"),
  libraryDependencies ++=
    "org.scalatest" %%% "scalatest" % "3.2.9" % Test ::
    "org.scalacheck" %% "scalacheck" % "1.15.4" % Test ::
    "org.scala-lang.modules" %%% "scala-collection-compat" % "2.5.0" ::
    Nil,
  autoAPIMappings := true,
  siteSubdirName in ScalaUnidoc := "latest/api",
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
)

lazy val core = (crossProject(JVMPlatform, JSPlatform) in file(".") / "core")
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js.settings(
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
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
  ).dependsOn(core)

lazy val layoutJS = layout.js
lazy val layoutJVM = layout.jvm

lazy val layoutElk = (crossProject(JVMPlatform, JSPlatform) in file(".") / "layout-elk")
  .settings(commonSettings)
  .settings(
    name := "graphs-layout-elk",
  ).jvmSettings(
  libraryDependencies ++= Seq(
    "org.eclipse.elk" % "org.eclipse.elk.alg.layered" % "0.7.1",
    "org.eclipse.elk" % "org.eclipse.elk.alg.mrtree" % "0.7.1"
  )
).dependsOn(layout)

lazy val layoutElkJS = layoutElk.js
lazy val layoutElkJVM = layoutElk.jvm

lazy val view = (crossProject(JVMPlatform, JSPlatform) in file(".") / "view")
  .settings(commonSettings)
  .settings(
    name := "graphs-view",
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.2.9",
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.4",
    libraryDependencies += "org.typelevel" %% "cats-effect-testing-scalatest" % "1.3.0" % Test
  ).jvmSettings(
    libraryDependencies += "org.apache.xmlgraphics" % "batik-rasterizer" % "1.14"
  ).dependsOn(core, layout, style)

lazy val viewJS = view.js
lazy val viewJVM = view.jvm

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
    name := "graphs-editor"
  ).dependsOn(core, view, graphml, json, cats)

lazy val editorJS = editor.js.settings(
  scalaJSUseMainModuleInitializer := true,
  artifactPath in (Compile, fastOptJS) := baseDirectory.value / ".." / "dist" / "app.js",
  artifactPath in (Compile, fullOptJS) := (artifactPath in (Compile, fastOptJS)).value
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
  libraryDependencies += "org.scalafx" %% "scalafx" % "15.0.1-R21",
  libraryDependencies += "org.fxmisc.richtext" % "richtextfx" % "0.10.5",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.14.0",
  libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.14.0",
  libraryDependencies ++= javaFXModules.map( m =>
    "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName
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
    ).map(_ % circeVersion)
  ).dependsOn(core)

lazy val jsonJS = json.js
lazy val jsonJVM = json.jvm

lazy val examples = (crossProject(JVMPlatform, JSPlatform) in file("examples"))
      .enablePlugins(ScalaJSBundlerPlugin)
      .settings(commonSettings)
      .settings(
        name := "graphs-examples",
        libraryDependencies ++= Seq(
          "org.typelevel" %%% "cats-core" % catsV
        )
      ).jsSettings(
        // command to create bundle: "examplesJS/fastOptJS::webpack"
        webpackBundlingMode := BundlingMode.LibraryAndApplication(),
        scalaJSUseMainModuleInitializer := false,
        Compile / npmDevDependencies += "elkjs" -> "0.7.1",
        Compile / npmDevDependencies += "web-worker" -> "^1.0.0",
        Compile / npmDependencies += "elkjs" -> "0.7.1",

        Compile / mainClass := Some("examples.LayoutExampleApp")
).dependsOn(core, graphml, cats, layout, layoutElk, json, editor)

lazy val examplesJS = examples.js
lazy val examplesJVM = examples.jvm

lazy val graphs = (project in file("."))
  .enablePlugins(ScalaUnidocPlugin)
  .settings(commonSettings)
  .settings(
    publishLocal := {},
    publish := {},
    test := {},
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(jsonJS, graphmlJS, coreJS, catsJS, layoutJS, editorJS, examplesJS, styleJS)
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
    layoutElkJS,
    layoutElkJVM,
    viewJS,
    viewJVM,
    editorJS,
    editorJVM,
    jsonJS,
    jsonJVM,
    styleJS,
    styleJVM
  )

lazy val docs = (project in file("docs"))
  .enablePlugins(ParadoxSitePlugin, GhpagesPlugin, ScalaUnidocPlugin)
  .settings(
    paradoxProperties += ("version" -> version.value),

    makeSite / mappings ++= Seq(
      file("LICENSE") -> "LICENSE",
      file("editor/dist/app.js") -> "editor/app.js",
      file("editor/dist/editor.html") -> "editor/index.html",
      file("editor/dist/config.json") -> "editor/config.json"
    ),

    git.remoteRepo := "git@github.com:flowtick/graphs.git",
    addMappingsToSiteDir(graphs / ScalaUnidoc / packageDoc / mappings, graphs / ScalaUnidoc / siteSubdirName)
  )
