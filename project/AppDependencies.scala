import sbt._

object AppDependencies {

  val bootstrapVersion = "8.5.0"
  val hmrcMongoVersion = "1.8.0"

  val compile = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"       %% "play-frontend-hmrc-play-30" % "9.1.0"
  )

  val test = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-30"    % hmrcMongoVersion   % "test",
    "org.scalamock"                %% "scalamock"                  % "5.2.0"            % "test",
    "uk.gov.hmrc"                  %% "bootstrap-test-play-30"     % bootstrapVersion   % "test",
  )

  val itDependencies = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-test-play-30" % bootstrapVersion  % Test,
  )
}
