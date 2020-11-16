How to cut a release
====================

## CI

graphs is using sbt-ci-release to publish artifacts.
To create a new release simply create a tag and push it:

```
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```

## Manually

Run 
  
    sbt "+release"
  
This needs valid credentials for the sonatype plugin:

```
$HOME/.sbt/(sbt-version 0.13 or 1.0)/sonatype.sbt
```

should contain

```
credentials += Credentials("Sonatype Nexus Repository Manager",
        "oss.sonatype.org",
        "(Sonatype user name)",
        "(Sonatype password)")
```

The cross release will sometimes enter a loop, just exist after the push.

Update Docs
===========

```
git checkout v<released_version>
sbt editorJS/fullOptJS # to create the editor js app in 'editor/dist'
sbt ghpagesPushSite
```

