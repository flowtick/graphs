enablePlugins(ParadoxMaterialThemePlugin, ParadoxSitePlugin, GhpagesPlugin)

sourceDirectory in Paradox := baseDirectory.value / "docs"

ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)

paradoxProperties += ("version" -> version.value)

mappings in makeSite ++= Seq(
  file("LICENSE") -> "LICENSE"
)

paradoxMaterialTheme in Paradox := {
  ParadoxMaterialTheme()
    .withColor("light-blue", "light-blue")
    .withCopyright("© graphs contributors")
    .withRepository(uri("https://github.com/flowtick/graphs"))
    .withFont("Source Sans Pro", "Iosevka")
    .withLogoIcon("linear_scale")
}

git.remoteRepo := "git@github.com:flowtick/graphs.git"

addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc)