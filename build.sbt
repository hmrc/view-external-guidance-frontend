/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import uk.gov.hmrc.DefaultBuildSettings.integrationTestSettings
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings

val appName = "view-external-guidance-frontend"
val silencerVersion = "1.7.1"

lazy val microservice = Project(appName, file("."))
  .enablePlugins(play.sbt.PlayScala, SbtDistributablesPlugin)
  .disablePlugins(JUnitXmlReportPlugin) //Required to prevent https://github.com/scalatest/scalatest/issues/1427
  .settings(
    scalaVersion := "2.13.8",
    majorVersion := 0,
    scalacOptions ++= Seq(
      "-feature",
      "-Wconf:src=routes/.*:s",
      "-Wconf:cat=unused-imports&src=html/.*:s"
    ),
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .configs(IntegrationTest)
  .settings(CodeCoverageSettings.settings: _*)
  .settings(integrationTestSettings(): _*)
  .settings(resolvers += Resolver.jcenterRepo)


inConfig(IntegrationTest)(org.scalafmt.sbt.ScalafmtPlugin.scalafmtConfigSettings)

TwirlKeys.templateImports ++= Seq(
  "uk.gov.hmrc.hmrcfrontend.views.html.components._",
  "uk.gov.hmrc.govukfrontend.views.html.components._",
)