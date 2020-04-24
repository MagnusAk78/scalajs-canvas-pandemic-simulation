enablePlugins(ScalaJSPlugin)

name := "scala.js-canvas-pandemic-simulation"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

// dom for node.js and scala.js
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

// uTest
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
