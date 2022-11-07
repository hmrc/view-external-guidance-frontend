import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-28"         % "0.73.0",
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.25.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "3.32.0-play-28"
  )

  val test = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-28"    % "0.73.0"        % "test",
    "org.scalamock"                %% "scalamock"                  % "5.2.0"         % "test",
    "org.jsoup"                    %  "jsoup"                      % "1.15.3"        % "test",
    "com.typesafe.play"            %% "play-test"                  % current         % "test",
    "org.scalatestplus.play"       %% "scalatestplus-play"         % "5.1.0"         % "test",
    "org.pegdown"                  %  "pegdown"                    % "1.6.0"         % "test, it",
    "com.github.tomakehurst"       %  "wiremock-jre8"              % "2.34.0"        % "test, it",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"       % "2.13.4"        % "test, it",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-28"     % "5.25.0"        % "test, it"
  )
}
