addSbtPlugin("com.scalapenos"       % "sbt-prompt"              % "2.0.0-SNAPSHOT")
addSbtPlugin("com.timushev.sbt"     % "sbt-updates"             % "0.6.3")
addDependencyTreePlugin
addSbtPlugin("ch.epfl.scala"        % "sbt-bloop"               % "1.5.3")

addCompilerPlugin("org.scalameta"   % "semanticdb-scalac"       % "4.5.12" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
