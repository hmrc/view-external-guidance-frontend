import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-28"         % "0.66.0",
    "uk.gov.hmrc"       %% "play-language"              % "5.3.0-play-28",
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.20.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "3.22.0-play-28"
  )

  val test = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-28"    % "0.66.0"        % "test",
    "org.scalamock"                %% "scalamock"                  % "5.2.0"         % "test",
    "org.jsoup"                    %  "jsoup"                      % "1.15.2"        % "test",
    "com.typesafe.play"            %% "play-test"                  % current         % "test",
    "org.scalatestplus.play"       %% "scalatestplus-play"         % "5.1.0"         % "test",
    "org.pegdown"                  %  "pegdown"                    % "1.6.0"         % "test, it",
    "com.github.tomakehurst"       %  "wiremock-jre8"              % "2.33.2"        % "test, it",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"       % "2.13.3"        % "test, it",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-28"     % "5.20.0"        % "test, it"
  )
}
