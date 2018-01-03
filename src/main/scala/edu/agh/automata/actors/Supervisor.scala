package edu.agh.automata.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import edu.agh.automata.actors.CellActor._
import edu.agh.automata.actors.WebsocketConnector.Close
import edu.agh.automata.models._
import edu.agh.automata.utils.Formatting
import org.scalacheck.Gen

class Supervisor(options: AutomataOptions, cellsPath: ActorRef, websocket: ActorRef) extends Actor with ActorLogging {
  import Supervisor._

  val numOfCells = options.sizeX * options.sizeY

  val sendMap =
    for {
      x <- 0 until options.sizeX
      y <- 0 until options.sizeY
    } yield (x, y)


  val stateGen: Gen[State] = {
    val healthy = (1 / options.initDiseased).toInt
    Gen.frequency(
      (healthy - 1, Healthy),
      (1, Infected)
    )
  }


  override def receive: Receive = {
    val initialMap = sendMap map { case (x, y) =>
      val nbrs = neighbours(x, y, options.sizeX, options.sizeY)
      val state = stateGen.sample.get
      val sendCoords = nbrs.map { case (nx, ny) => Metadata(nx, ny, options.sizeX, options.sizeY) }
      cellsPath ! CellMessageEnvelope(Metadata(x, y, options.sizeX, options.sizeY),
        Initialize(CellData(nbrs.length, sendCoords, options, cellsPath, self), state)
      )
      StateChange(x, y, state)
    }
    websocket ! AllChanges(initialMap)

    standBy(frame = 0)
  }

  def standBy(frame: Int): Receive = {
    case StartNextFrame => //todo: case object
      sendMap foreach { case (x, y) =>
        cellsPath ! CellMessageEnvelope(Metadata(x, y, options.sizeX, options.sizeY), StartNewFrame(frame))
      }
      val currentTime = System.nanoTime()
      context become awaitResponse(frame, Seq.empty, 0, currentTime)

    case msg =>
      log.warning("INVALID MESSAGE {} IN STATE STANDBY", msg)
  }

  def awaitResponse(frame: Int, changes: Seq[StateChange], numOfResponses: Int, startTime: Long): Receive = {
    case CellStateChange(x, y, change) =>
      val newChanges = change.map(st => StateChange(x, y, st) +: changes).getOrElse(changes)
      val newNumOfResponses = numOfResponses + 1
      if (newNumOfResponses == numOfCells) {
        websocket ! AllChanges(newChanges)
        if ((frame + 1) >= options.length)  {
          websocket ! Close
          sendMap foreach { case (nx, ny) =>
            cellsPath ! CellMessageEnvelope(Metadata(nx, ny, options.sizeX, options.sizeY), Reset)
          }
          context.stop(self)
        }
        val dt = System.nanoTime() - startTime
        log.info("Finished frame {}/{}, took {}", frame, options.length, Formatting.formatDuration(dt))
        context become standBy(frame + 1)
      } else {
        context become awaitResponse(frame, newChanges, newNumOfResponses, startTime)
      }

    case msg =>
      log.warning("INVALID MESSAGE {} IN STATE awaitResponse", msg)
  }



}

object Supervisor {
  def props(options: AutomataOptions, cellsPath: ActorRef, websocket: ActorRef) = Props(new Supervisor(options, cellsPath, websocket))

  def neighbours(x: Int, y: Int, sizeX: Int, sizeY: Int): Seq[(Int, Int)] = {
    val fs: Seq[Int => Int] = Seq(_ - 1, identity, _ + 1)
    val xs = fs.map(_(x))
    val ys = fs.map(_(y))
    (xs cross ys)
      .filter { _ != ((x, y)) }
      .filter { case (nX, nY) => (0 until sizeX contains nX) && (0 until sizeY contains nY) }
  }

  case class AllChanges(changes: Seq[StateChange])
  case object StartNextFrame
  case object Shutdown

  implicit class Cross[A](as: Seq[A]) {
    def cross[B](bs: Seq[A]): Seq[(A, A)] = for {
      a <- as
      b <- bs
    } yield (a, b)
  }
}