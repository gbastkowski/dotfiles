addDependencyTreePlugin

// addSbtPlugin("com.scalapenos"     % "sbt-prompt"        % "1.0.2")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"       % "0.6.4")

addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.9.4" cross CrossVersion.full)
scalacOptions += "-Yrangepos"
