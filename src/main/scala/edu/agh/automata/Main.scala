package edu.agh.automata

import akka.actor.ActorSystem
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.StrictLogging
import edu.agh.automata.actors.CellActor
import edu.agh.automata.api.AutomataCtrl
import edu.agh.automata.utils.Settings

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object Main extends App with StrictLogging {

  implicit val as = ActorSystem("automaton")
  implicit val executionContext: ExecutionContextExecutor = as.dispatcher
  implicit val mat = ActorMaterializer()

  val name = "cell"

  ClusterSharding(as).start(
    typeName = name,
    entityProps = CellActor.props(),
    settings = ClusterShardingSettings(as),
    extractEntityId = CellActor.extractEntityId,
    extractShardId = CellActor.extractShardId
  )

  val cellsPath = ClusterSharding(as).shardRegion(name)

  val ctrl = new AutomataCtrl(as, cellsPath)

  Http().bindAndHandle(ctrl.endpoints, Settings.host, Settings.port).onComplete {
    case Success(address) => logger.info("Successfully bound to [{}]", address)
    case Failure(ex)      => logger.error("Couldn't connect [{}]", ex)
  }

}
