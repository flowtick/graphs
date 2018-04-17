How to cut a release
====================

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
