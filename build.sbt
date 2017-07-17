val mainScalaVersion = "2.12.2"

lazy val commonSettings = Seq(
  organization := "com.flowtick",
  scalaVersion := mainScalaVersion,
  crossScalaVersions := Seq(mainScalaVersion, "2.11.11", "2.10.6"),
  releaseCrossBuild := true,
  libraryDependencies ++=
    "org.scalatest" %% "scalatest" % "3.0.1" % Test ::
    "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0" % Test ::
    Nil,
  wartremoverErrors ++= Warts.unsafe.filterNot(Seq(
    Wart.NonUnitStatements,
    Wart.DefaultArguments,
    Wart.Any
  ).contains(_)),
  bintrayOrganization := Some("flowtick"),
  bintrayRepository := "graphs",
  licenses += ("Apache-2.0", url("https://opensource.org/licenses/apache-2.0"))
)

lazy val core = (crossProject in file(".") / "core")
  .enablePlugins(SiteScaladocPlugin)
  .settings(commonSettings)
  .settings(
    name := "graphs-core"
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val graphs = (project in file("."))
  .enablePlugins(ParadoxSitePlugin)
  .settings(commonSettings)
  .settings(
    publishLocal := {},
    publish := {},
    test := {},
    sourceDirectory in Paradox := baseDirectory.value / "docs",
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    mappings in makeSite ++= Seq(
      file("LICENSE") -> "LICENSE"
    )
  ).aggregate(coreJS, coreJVM)

lazy val updateDocs = taskKey[Unit]("push docs to https://flowtick.bitbucket.io")

updateDocs := {
  val tempSite = file("target") / "flowtick-site"
  IO.delete(tempSite)
  s"git clone git@bitbucket.org:flowtick/flowtick.bitbucket.io.git ${tempSite.absolutePath}".!

  val siteDir = (makeSite in graphs).value
  val scalaDocDir = (makeSite in coreJVM).value / "latest" / "api"

  IO.copyDirectory(siteDir, tempSite / "graphs")
  IO.copyDirectory(scalaDocDir, tempSite / "graphs" / "api")

  Process("git add .", tempSite).!
  Process(Seq("git", "commit", "-m", "'update docs'"), tempSite).!
  Process(Seq("git", "push", "origin", "master", "--force"), tempSite).!
}

addCommandAlias("testWithCoverage", ";clean;coverage;test;coverageReport")