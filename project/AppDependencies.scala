import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc" %% "simple-reactivemongo" % "7.31.0-play-26",
    "uk.gov.hmrc" %% "play-language" % "4.7.0-play-27",
    "uk.gov.hmrc" %% "bootstrap-frontend-play-27" % "3.4.0",
    "uk.gov.hmrc" %% "play-frontend-govuk" % "0.60.0-play-27",
    "uk.gov.hmrc" %% "play-frontend-hmrc" % "0.38.0-play-27",
    "uk.gov.hmrc" %% "auth-client" % "3.3.0-play-27"
  )

  val test = Seq(
    "uk.gov.hmrc" %% "reactivemongo-test" % "4.22.0-play-26",
    "uk.gov.hmrc" %% "bootstrap-frontend-play-27" % "3.4.0" % "test",
    "org.scalamock" %% "scalamock" % "4.4.0" % "test",
    "org.scalatest" %% "scalatest" % "3.0.8" % "test",
    "org.jsoup" % "jsoup" % "1.13.1" % "test",
    "com.typesafe.play" %% "play-test" % current % "test",
    "org.pegdown" % "pegdown" % "1.6.0" % "test, it",
    "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.0" % "test, it",
    "com.github.tomakehurst" % "wiremock" % "2.23.2" % "test, it"
  )

  // Fixes a transitive dependency clash between wiremock and scalatestplus-play
  val overrides: Seq[ModuleID] = {
    val jettyFromWiremockVersion = "9.2.24.v20180105"
    Seq(
      "org.eclipse.jetty" % "jetty-client" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-continuation" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-http" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-io" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-security" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-servlet" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-servlets" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-util" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-webapp" % jettyFromWiremockVersion,
      "org.eclipse.jetty" % "jetty-xml" % jettyFromWiremockVersion,
      "org.eclipse.jetty.websocket" % "websocket-api" % jettyFromWiremockVersion,
      "org.eclipse.jetty.websocket" % "websocket-client" % jettyFromWiremockVersion,
      "org.eclipse.jetty.websocket" % "websocket-common" % jettyFromWiremockVersion
    )
  }

}
