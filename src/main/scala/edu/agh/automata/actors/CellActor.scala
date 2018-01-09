package edu.agh.automata.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import edu.agh.automata.actors.CellActor._
import edu.agh.automata.models._

import scala.util.Random

class CellActor extends Actor with ActorLogging {

  val (x, y) = extractCoordinates(self.path.name)

  override def receive: Receive = initial

  def initial: Receive = {
    case Initialize(cellData, initialState) =>
      context become standby(Seq.empty, 0, initialState, cellData)
    case msg =>
      log.warning("INVALID MESSAGE {} IN STATE INITIAL", msg)
  }

  def standby(collected: Seq[State], frame: Int, state: State, cellData: CellData): Receive = {
    case StartNewFrame(`frame`) =>
      cellData.neighbours foreach { metadata =>
        val msg = SendBorder(state)
        cellData.cellsPath ! CellMessageEnvelope(metadata, msg)
      }
      //todo gotta check stuff
      context become standby(collected, frame, state, cellData)

    case SendBorder(neighbour) =>
      val newCollected = neighbour +: collected
      if (newCollected.length < cellData.numOfNeighbours) {
        context become standby(newCollected, frame, state, cellData)
      } else {
        val newState = CellActor.checkChangeDelta(state, collected, cellData.options.p, cellData.options.q, cellData.options.i)
        cellData.supervisor ! CellStateChange(x, y, newState)
        context become standby(Seq.empty, frame + 1, newState.getOrElse(state), cellData)
      }

    case Reset =>
      context stop self
    case msg =>
      log.warning("INVALID MESSAGE {} IN STATE STANDBY, state {}, frame {}", msg, state, frame)
      context stop self
  }

}

object CellActor {
  def props() = Props(new CellActor)

  case class CellData(numOfNeighbours: Int,
                      neighbours: Seq[Metadata],
                      options: AutomataOptions,
                      cellsPath: ActorRef,
                      supervisor: ActorRef)

  val shardNum: Int = 2 //todo: config given value

/*  sealed trait Direction
  case object N extends Direction
  case object S extends Direction
  case object E extends Direction
  case object W extends Direction
  case object NE extends Direction
  case object NW extends Direction
  case object SE extends Direction
  case object SW extends Direction*/

  def checkChangeDelta(state: State, neighbours: Seq[State], p: Double, q: Double, i: Int): Option[State] = state match {
    case Healthy =>
      val r = neighbours.count(_ == Infected)
      val chance = 1 - math.pow(1 - q, r.toDouble)
      if (Random.nextDouble() < chance) Some(Infected)
      else None

    case Infected =>
      val chance = p
      if (Random.nextDouble() > chance) Some(Immune(i))
      else None

    case Immune(0) =>
      Some(Healthy)

    case Immune(n) =>
      Some(Immune(n - 1))
  }

  def extractShardId: ExtractShardId = {
    case CellMessageEnvelope(d, _) =>
      val xCoords = d.x / (d.dimX / shardNum)
      val yCoords = d.y / (d.dimY / shardNum)
      (xCoords + yCoords * shardNum).toString
  }

  def extractEntityId: ExtractEntityId = {
    case CellMessageEnvelope(metadata, msg) => (s"${metadata.x}x${metadata.y}", msg)
  }

  def extractCoordinates(entityId: String) = {
    val x +: y +: Nil = entityId.split('x').map(_.toInt).toSeq
    (x, y)
  }

  case class CellMessageEnvelope(metadata: Metadata, message: CellMessage)
  sealed trait CellMessage
  case class Metadata(x: Int, y: Int, dimX: Int, dimY: Int)

  case class Initialize(cellData: CellData, initialState: State) extends CellMessage
  case class StartNewFrame(frame: Int) extends CellMessage
  case class SendBorder(border: State) extends CellMessage

  case object Reset extends CellMessage

  case class CellStateChange(x: Int, y: Int, change: Option[State])

}