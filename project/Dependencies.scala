import sbt._

object Dependencies {
	object Versions {
		lazy val scalatest = "3.0.5"
		lazy val scalacheck = "1.14.0"
	}

	lazy val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatest
	lazy val scalacheck = "org.scalacheck" %% "scalacheck" % Versions.scalacheck
}