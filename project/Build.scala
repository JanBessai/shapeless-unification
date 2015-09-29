import sbt._
import Keys._

object Dependencies {
  val scala_test = "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.3"
  val shapeless = "com.chuusai" %% "shapeless" % "2.2.5"
}

object ShapelessUnificationBuild extends Build {
  import Dependencies._

  lazy val semanticSettings =
    Seq(
      organization := "de.tu_dortmund.cs.ls14",
      description := "Generic unification algorithm using shapeless"
    )

  lazy val technicalSettings =
    Seq(
      scalacOptions :=
        Seq(
          "-feature",
          "-unchecked",
          "-deprecation"/*,
          "-Xlog-implicits"*/
        ),
      scalaVersion := "2.11.7",
      libraryDependencies ++=
        Seq(
          scala_test,
          scalaz,
          shapeless
        )
    )

  lazy val shapelessUnification = (project in file(".")
    aggregate (core, examples)
    dependsOn (core, examples, scratch)
    settings (semanticSettings: _*)
    settings (technicalSettings: _*)
    settings (
      name := "shapeless-unification",
      moduleName := "shapless-unification-root",
      publish := (),
      publishLocal := ()
    )
  )

  lazy val core = (project
    settings (semanticSettings: _*)
    settings (technicalSettings: _*)
    settings (
      moduleName := "shapeless-unification"
    )
  )

  lazy val examples = (project
    dependsOn core
    settings (semanticSettings: _*)
    settings (technicalSettings: _*)
    settings (
      moduleName := "shapeless-unification-examples",
      publish := (),
      publishLocal := ()
    )
  )

  lazy val scratch = (project
    dependsOn (core, examples)
    settings (semanticSettings: _*)
    settings (technicalSettings: _*)
    settings (
      moduleName := "shapeless-unification-scratch",
      publish := (),
      publishLocal := ()
    )
  )


}
