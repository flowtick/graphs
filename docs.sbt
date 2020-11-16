import sbt.file

enablePlugins(ParadoxMaterialThemePlugin, ParadoxSitePlugin, GhpagesPlugin)

sourceDirectory in Paradox := baseDirectory.value / "docs"

ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)

paradoxProperties += ("version" -> version.value)

mappings in makeSite ++= Seq(
  file("LICENSE") -> "LICENSE",
  file("editor/dist/app.js") -> "editor/app.js",
  file("editor/dist/editor.html") -> "editor/index.html",
  file("editor/dist/config.json") -> "editor/config.json"
)

paradoxMaterialTheme in Paradox := {
  ParadoxMaterialTheme()
    .withColor("green", "green")
    .withCopyright("© graphs contributors")
    .withRepository(uri("https://github.com/flowtick/graphs"))
    .withFont("Source Sans Pro", "Iosevka")
    .withLogoIcon("linear_scale")
}

git.remoteRepo := "git@github.com:flowtick/graphs.git"

addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc)