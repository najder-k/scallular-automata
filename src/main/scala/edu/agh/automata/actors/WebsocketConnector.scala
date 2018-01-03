package edu.agh.automata.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import edu.agh.automata.actors.Supervisor.{AllChanges, StartNextFrame}
import edu.agh.automata.actors.WebsocketConnector.{Close, RegisterReceiver}
import edu.agh.automata.models.AutomataOptions
import edu.agh.automata.serializers.JsonSupport

class WebsocketConnector(cellsPath: ActorRef) extends Actor with ActorLogging with JsonSupport {
  import spray.json._

  override def receive: Receive = initial

  def initial: Receive = {
    case RegisterReceiver(actr) =>
      context become awaitingOptions(actr)
  }

  def awaitingOptions(receiver: ActorRef): Receive = {
    case msg: TextMessage =>
      val opts: String = msg.getStrictText
      val options = opts.parseJson.convertTo[AutomataOptions]
      val supervisor = context.actorOf(Supervisor.props(options, cellsPath, self))

      context become simulationInProgress(supervisor, receiver)

    case Close =>
      context.stop(receiver)
      context.stop(self)
  }

  def simulationInProgress(supervisor: ActorRef, receiver: ActorRef): Receive = {
    case AllChanges(changes) =>
      receiver ! TextMessage(changes.toJson.compactPrint)
      supervisor ! StartNextFrame
    case _: TextMessage => //ignore

    case Close =>
      context.stop(receiver)
      context.stop(supervisor)//todo: poisonpill?
      context.stop(self)
  }
}

object WebsocketConnector {
  case class RegisterReceiver(receiver: ActorRef)

  case object Close

  def props(cellsPath: ActorRef) = Props(new WebsocketConnector(cellsPath))

  val messageBufferSize: Int = 1000

  def messageFlow(connector: ActorRef)(implicit ac: ActorMaterializer): Flow[Message, Message, Any] = {
    val fromWebsocket = Flow[Message]
    val actorSink     = Sink.actorRef[Message](connector, onCompleteMessage = Close)
    val actorSource   = Source.actorRef[Message](messageBufferSize, OverflowStrategy.fail)
      .mapMaterializedValue(connector ! RegisterReceiver(_))

    Flow.fromSinkAndSource[Message, Message](fromWebsocket to actorSink, actorSource)
  }

}