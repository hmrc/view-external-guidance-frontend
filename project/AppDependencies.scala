import sbt._

object AppDependencies {

  val bootstrapVersion = "8.2.0"

  val compile = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-28"         % "1.6.0",
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-28" % bootstrapVersion,
    "uk.gov.hmrc"       %% "play-frontend-hmrc"         % "7.29.0-play-28"
  )

  val test = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-28"    % "1.6.0"        % "test",
    "org.scalamock"                %% "scalamock"                  % "5.2.0"         % "test",
    "org.jsoup"                    %  "jsoup"                      % "1.17.1"        % "test",
    "org.pegdown"                  %  "pegdown"                    % "1.6.0"         % "test",
    "com.github.tomakehurst"       %  "wiremock-jre8"              % "2.35.1"        % "test",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"       % "2.16.0"        % "test",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-28"     % bootstrapVersion        % "test"
  )

  val itDependencies = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-test-play-28" % bootstrapVersion  % Test,
    "org.pegdown"                   %  "pegdown"                % "1.6.0"           % Test,
    "com.github.tomakehurst"        % "wiremock-jre8"           % "2.35.1"          % Test,
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"   % "2.16.0"          % Test,
    "uk.gov.hmrc"                   %% "bootstrap-test-play-28" % bootstrapVersion  % Test
  )
}
