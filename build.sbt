name := "CLS-Servo-Robot"

version := "0.1"

scalaVersion := "2.13.5"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  //"org.combinators" %% "shapeless-feat" % "0.2.5",
  "org.combinators" %% "cls-scala" % "3.0.0",
  "io.netty" % "netty-buffer" % "4.1.63.Final"
)




