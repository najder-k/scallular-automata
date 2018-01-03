package edu.agh.automata.utils

import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration.FiniteDuration

object Settings {
  val config: Config = ConfigFactory.load()

  val host: String = config.getString("api.host")
  val port: Int = config.getInt("api.port")

  val numberOfShards: Int = config.getInt("cluster.shardNumber")

  val timeout: FiniteDuration = config.getDuration("api.timeout")

  implicit def durationToFinite(d: java.time.Duration): FiniteDuration =
    scala.concurrent.duration.Duration.fromNanos(d.toNanos)
}
