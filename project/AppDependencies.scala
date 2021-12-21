import play.core.PlayVersion.current
import sbt._

object AppDependencies {

  val compile = Seq(
    //"uk.gov.hmrc" %% "simple-reactivemongo"       % "8.0.0-play-28",
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-28"         % "0.58.0",
    "uk.gov.hmrc"       %% "play-language"              % "5.1.0-play-28",
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % "5.18.0",
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "1.31.0-play-28"
  )

  val test = Seq(
    //"uk.gov.hmrc"                  %% "reactivemongo-test"         % "5.0.0-play-28" % "test",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-27"    % "0.58.0"        % "test",
    "org.scalamock"                %% "scalamock"                  % "5.1.0"         % "test",
    "org.jsoup"                    %  "jsoup"                      % "1.14.3"        % "test",
    "com.typesafe.play"            %% "play-test"                  % current         % "test",
    "org.scalatestplus.play"       %% "scalatestplus-play"         % "5.1.0"         % "test",
    "org.pegdown"                  %  "pegdown"                    % "1.6.0"         % "test, it",
    "com.github.tomakehurst"       %  "wiremock-jre8"              % "2.32.0"        % "test, it",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"       % "2.13.0"        % "test, it",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-28"     % "5.18.0"        % "test, it"
  )
}
