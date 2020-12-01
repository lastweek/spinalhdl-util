name := "SpinalTemplateProject"

version := "1.0"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.1",
  "com.github.spinalhdl" % "spinalhdl-core_2.11" % "latest.release",
  "com.github.spinalhdl" % "spinalhdl-lib_2.11" % "latest.release",
  "org.scodec" %% "scodec-bits" % "1.1.12",
  "org.scodec" %% "scodec-core" % "1.11.4",

  compilerPlugin("com.github.spinalhdl" % "spinalhdl-idsl-plugin_2.11" % "1.4.0")
)
Test / scalaSource := baseDirectory.value / "src/main/scala/supernic/test"
Test / unmanagedSourceDirectories += baseDirectory.value / "src/main/scala/apps/test"

fork := true
