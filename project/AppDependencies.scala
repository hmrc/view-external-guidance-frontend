import sbt._

object AppDependencies {

  val bootstrapVersion = "8.6.0"
  val hmrcMongoVersion = "1.9.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "uk.gov.hmrc"       %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"       %% "play-frontend-hmrc-play-30" % "10.3.0"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-test-play-30"    % hmrcMongoVersion   % Test,
    "org.scalamock"                %% "scalamock"                  % "5.2.0"            % Test,
    "uk.gov.hmrc"                  %% "bootstrap-test-play-30"     % bootstrapVersion   % Test,
  )

  val itDependencies: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                   %% "bootstrap-test-play-30" % bootstrapVersion  % Test,
  )
}
