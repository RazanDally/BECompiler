import Wart._

lazy val akkaVersion = "2.9.3"
lazy val akkaHttpVersion = "10.6.3"

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

ThisBuild / scalaVersion     := "2.13.9"
ThisBuild / organization     := "io.lptk"
ThisBuild / organizationName := "LPTK"


lazy val root = project.in(file(".")).settings(
  name := "hmloc",
  version := "0.1",
  scalaVersion := "2.13.9",
  Compile / mainClass := Some("hmloc.MainApp"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-language:higherKinds",
    "-Ywarn-value-discard",
  ),
  scalacOptions ++= {
    if (insideCI.value) Seq("-Wconf:any:error")
    else                Seq("-Wconf:any:warning")
  },
  wartremoverWarnings ++= Warts.allBut(
    Recursion, Throw, Nothing, Return, While, IsInstanceOf,
    Var, MutableDataStructures, NonUnitStatements,
    DefaultArguments, ImplicitParameter, ImplicitConversion,
    StringPlusAny, Any,
    JavaSerializable, Serializable, Product,
    LeakingSealed, Overloading,
    Option2Iterable, ListAppend
    , PublicInference
  ),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test,
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.0",
  libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.3.3",
  libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0",
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http"                % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-http-spray-json"     % akkaHttpVersion,
    "com.typesafe.akka" %% "akka-actor-typed"         % akkaVersion,
    "com.typesafe.akka" %% "akka-stream"              % akkaVersion,
    "ch.qos.logback"    % "logback-classic"           % "1.2.11",
  ),

  //
  watchSources += WatchSource(
    sourceDirectory.value.getParentFile.getParentFile/"src/test/diff", "*.ml", NothingFilter),
  watchSources += WatchSource(
    sourceDirectory.value.getParentFile.getParentFile/"src/test/diff", "*.mls", NothingFilter),
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oC"),
)
