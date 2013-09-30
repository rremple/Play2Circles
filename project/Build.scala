import sbt._
import Keys._
import play.Project._
//import cloudbees.Plugin._

object ApplicationBuild extends Build {

  val appName         = "Play2Circles"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm
  )

  val main = play.Project(appName, appVersion, appDependencies)
//    .settings(cloudBeesSettings :_*)
//    .settings(CloudBees.applicationId := Some("your account name/your app name"))

}
