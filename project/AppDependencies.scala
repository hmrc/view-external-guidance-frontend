import sbt._

object AppDependencies {

  val bootstrapVersion = "8.4.0"

  val compile = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-30"         % "1.7.0",
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"       %% "play-frontend-hmrc-play-30" % "8.5.0"
  )

  val test = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-30"    % "1.7.0"        % "test",
    "org.scalamock"                %% "scalamock"                  % "6.0.0-M1"     % "test",
    "org.jsoup"                    %  "jsoup"                      % "1.17.2"       % "test",
    "org.pegdown"                  %  "pegdown"                    % "1.6.0"        % "test",
    "com.github.tomakehurst"       %  "wiremock-jre8"              % "3.0.1"        % "test",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"       % "2.16.1"       % "test",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-30"     % bootstrapVersion % "test",
    "uk.gov.hmrc"                  %% "play-frontend-hmrc-play-30" % "8.5.0"        % "test",
    "uk.gov.hmrc"                  %% "http-verbs-play-30"         % "14.12.0"        % "test"
  )

  val itDependencies = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-test-play-30" % bootstrapVersion  % Test,
    "org.pegdown"                   %  "pegdown"                % "1.6.0"           % Test,
    "com.github.tomakehurst"        % "wiremock-jre8"           % "3.0.1"           % Test,
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"   % "2.16.1"          % Test,
    "uk.gov.hmrc"                   %% "play-frontend-hmrc-play-30" % "8.5.0"        % Test
  )
}
