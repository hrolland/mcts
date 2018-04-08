name := "mcts"

version := "0.1-SNASPHOT"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
	//	"org.scalatest" %% "scalatest" % "2.1.6" % "test",
	"org.specs2" %% "specs2-core" % "4.0.0" % "test",
	"org.specs2" %% "specs2-mock" % "4.0.0" % "test",
	"org.mockito" % "mockito-all" % "1.9.5"% "test",
	"commons-io" % "commons-io" % "2.0.1" % "test"
)
