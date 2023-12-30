addDependencyTreePlugin

addSbtPlugin("com.scalapenos"     % "sbt-prompt"        % "2.0.0-SNAPSHOT")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"       % "0.6.4")
// addSbtPlugin("ch.epfl.scala"      % "sbt-bloop"         % "1.5.13")

addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.8.14" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
