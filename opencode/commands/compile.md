---
description: Compile the current project using the appropriate build tool
---

# Compile Project
Compiles the current project by detecting the build system and running the appropriate compile command.

## Detection and Commands:
If AGENTS.md has instructions, follow them. Otherwise:
- **SBT projects** (build.sbt or project/ directory): `sbt compile`
- **Maven projects** (pom.xml): `mvn compile`
- **Gradle projects** (build.gradle or build.gradle.kts): `./gradlew compileJava` or `gradle compileJava`
- **npm projects** (package.json with build script): `npm run build`
- **Make projects** (Makefile): `make`

Automatically detects the project type and runs the appropriate compile command.
