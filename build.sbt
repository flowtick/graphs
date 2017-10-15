val mainScalaVersion = "2.12.3"

lazy val commonSettings = Seq(
  organization := "com.flowtick",
  scalaVersion := mainScalaVersion,
  crossScalaVersions := Seq(mainScalaVersion, "2.11.11"),
  releaseCrossBuild := true,
  libraryDependencies ++=
    "org.scalatest" %%% "scalatest" % "3.0.4" % Test ::
    "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % Test ::
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
  homepage := Some(url("https://flowtick.bitbucket.io/graphs")),
  scmInfo := Some(
    ScmInfo(
      url("https://bitbucket.org/flowtick/graphs.git"),
      "scm:git@bitbucket.org:flowtick/graphs.git"
    )
  ),
  developers := List(
    Developer(id="adrobisch", name="Andreas D.", email="github@drobisch.com", url=url("http://drobisch.com/"))
  )
)

lazy val core = (crossProject in file(".") / "core")
  .enablePlugins(SiteScaladocPlugin)
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val graphml = (crossProject in file(".") / "graphml")
  .enablePlugins(SiteScaladocPlugin)
  .settings(commonSettings)
  .settings(
    name := "graphs-graphml"
  ).jvmSettings(
    resolvers += Resolver.bintrayRepo("flowtick", "jgraphx"),
    libraryDependencies ++= Seq(
      "com.mxgraph" % "jgraphx" % "3.7.4",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
    )
  ).dependsOn(core)

lazy val graphmlJS = graphml.js
lazy val graphmlJVM = graphml.jvm

lazy val examples = (project in file("examples"))
      .settings(commonSettings)
      .dependsOn(coreJVM, graphmlJVM)

lazy val graphs = (project in file("."))
  .enablePlugins(ParadoxSitePlugin)
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
    )
  ).aggregate(coreJS, coreJVM, examples, graphmlJS, graphmlJVM)

lazy val updateDocs = taskKey[Unit]("push docs to https://flowtick.bitbucket.io")

updateDocs := {
  import scala.sys.process._

  val tempSite = file("target") / "flowtick-site"
  IO.delete(tempSite)
  s"git clone git@bitbucket.org:flowtick/flowtick.bitbucket.io.git ${tempSite.absolutePath}".!

  val siteDir = (makeSite in graphs).value
  val scalaDocDir = (makeSite in coreJVM).value / "latest" / "api"

  IO.copyDirectory(siteDir, tempSite / "graphs", overwrite = true)
  IO.copyDirectory(scalaDocDir, tempSite / "graphs" / "api", overwrite = true)

  Process("git add .", tempSite).!
  Process(Seq("git", "commit", "-m", "'update docs'"), tempSite).!
  Process(Seq("git", "push", "origin", "master", "--force"), tempSite).!
}

addCommandAlias("testWithCoverage", ";clean;coverage;test;coverageReport")