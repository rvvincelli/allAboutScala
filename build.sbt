name := "AllAboutScala"

scalaVersion := "2.10.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.chuusai" % "shapeless_2.10.3" % "2.0.0",
  "com.netflix.rxjava" % "rxjava-scala" % "0.16.1",
  "com.typesafe.akka" %% "akka-actor" % "2.2.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.4",
  "org.scala-lang.modules" %% "scala-async" % "0.9.1",
  "org.scala-lang" % "scala-reflect" % "2.10.3",
  "org.scala-lang" % "scala-compiler" % "2.10.3"
)
