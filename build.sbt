
// The UI, which is in Scala.js. You won't need to modify the client.
lazy val ui = project.in(file("ui"))
  .settings(
    name := "ui",
    version := "2025.0",
    scalaVersion := "3.3.5",

    // This means the Scala to turn into JS has a main method that we want run when the JS is loaded
    scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseMainModuleInitializer := false,

    // The UI just depends on my little UI framework
    libraryDependencies ++= Seq(
      "com.wbillingsley" %%% "doctacular" % "0.3.0",
    )
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)

// The server, which runs on the JVM.
lazy val server = (project in file("server"))
  .aggregate(ui)     // Include the files from the UI, because the server has to send the browser the JS file
  .settings(
    name := "server",
    version := "2025.0",
    scalaVersion := "3.3.5",

    // The server is where we have Pekko and Pekko-HTTP
    libraryDependencies ++= Seq(
        "org.apache.pekko" %% "pekko-stream" % "1.1.3",
        "org.apache.pekko" %% "pekko-actor-typed" % "1.1.3",
        "org.apache.pekko" %% "pekko-http" % "1.1.0",

        "org.apache.pekko" %% "pekko-http-spray-json" % "1.1.0",

        "org.scalameta" %% "munit" % "0.7.29" % Test, 
    ), 

    // When we edit code in the UI, we're not altering any of the server code. But we still want to ensure
    // that if we recompile the server, we recompile the UI (otherwise we'd get confused why we're seeing outdated JS)
    // This bit tells SBT to make sure it regenerates the UI (if it's changed) when we recompile the server code.
    scalaJSProjects := Seq(ui),
    Assets / pipelineStages := Seq(scalaJSPipeline),
    pipelineStages := Seq(scalaJSPipeline),
    Runtime / managedClasspath += (Assets / packageBin).value,
    Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,

  ).enablePlugins(SbtWeb, JavaAppPackaging)

lazy val aggregate = (project in file(".")).aggregate(server, ui)

// We also need to register munit as a test framework in sbt so that "sbt test" will work and the IDE will recognise
// tests
testFrameworks += new TestFramework("munit.Framework")
