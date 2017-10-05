name := "sparkSessionTest"

version := "0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.0"
libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.1.0"
// https://mvnrepository.com/artifact/org.apache.spark/spark-mllib_2.10
//libraryDependencies += "org.apache.spark" % "spark-mllib_2.10" % "2.1.0" % "provided"
libraryDependencies += "org.apache.spark" %% "spark-mllib" % "2.1.0"