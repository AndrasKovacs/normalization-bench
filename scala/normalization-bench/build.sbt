lazy val root = (project in file(".")).
  enablePlugins(JmhPlugin).
  settings(
    name := "normalization-bench",
    version := "1.0",
    scalaVersion := "2.13.2"
  )
