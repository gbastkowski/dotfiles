addSbtPlugin("com.scalapenos"       % "sbt-prompt"              % "2.0.0-SNAPSHOT")
addSbtPlugin("com.timushev.sbt"     % "sbt-updates"             % "0.6.0")
addDependencyTreePlugin
addSbtPlugin("ch.epfl.scala"        % "sbt-bloop"               % "1.5.0-26-a132042c")

addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.5.10" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
