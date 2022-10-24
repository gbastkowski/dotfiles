addSbtPlugin("com.scalapenos"       % "sbt-prompt"              % "2.0.0-SNAPSHOT")
addSbtPlugin("com.timushev.sbt"     % "sbt-updates"             % "0.6.4")
addDependencyTreePlugin
addSbtPlugin("ch.epfl.scala"        % "sbt-bloop"               % "1.5.4")

addCompilerPlugin("org.scalameta"   % "semanticdb-scalac"       % "4.6.0" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
