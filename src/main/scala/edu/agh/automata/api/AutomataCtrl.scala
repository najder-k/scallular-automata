package edu.agh.automata.api

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.{Directives, Route}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.util.Timeout
import com.typesafe.scalalogging.StrictLogging
import edu.agh.automata.actors.WebsocketConnector
import edu.agh.automata.models.AutomataState
import edu.agh.automata.serializers.JsonSupport
import edu.agh.automata.utils.Settings


class AutomataCtrl(as: ActorSystem, cellsPath: ActorRef) extends Directives with JsonSupport with StrictLogging {
  implicit val ac = as
  implicit val timeout: Timeout = Settings.timeout
  implicit val mat = ActorMaterializer()

  def endpoints: Route =
    path("automata") {
      logger.debug("got a connection to automata")
      handleWebSocketMessages(WebsocketConnector.messageFlow(ac.actorOf(WebsocketConnector.props(cellsPath))))
    } ~
    path("oldautomata" / DoubleNumber / DoubleNumber / IntNumber / DoubleNumber / IntNumber / IntNumber / IntNumber) { (p, q, immunity, initDiseased, length, sizeX, sizeY) =>
      handleWebSocketMessages(startAutomata(p, q, immunity, initDiseased, length, sizeX, sizeY))
    }


  private def startAutomata(p: Double, q: Double, immunity: Int, initDiseased: Double,
                            length: Int, sizeX: Int, sizeY: Int): Flow[Message, Message, Any] = {
    val mock = new AutomataState(
      sizeX = sizeX,
      sizeY = sizeY,
      p = p,
      q = q,
      immunity = immunity,
      initDiseased = initDiseased
    )

    val simulation = Source.unfold(mock.init() -> length) {
      case (_, 0) => None
      case (state, _) if mock.isDead(state) => None
      case (state, `length`) =>
        val theWholeState = mock.json(state)
        Some(state -> (length - 1) -> TextMessage(theWholeState))
      case (state, len) =>
        val newState = mock.step(state)
        val delta = mock.json(mock.delta(state, newState))
        Some(newState -> (len - 1) -> TextMessage(delta))
    }

    Flow.fromSinkAndSource[Message, Message](Sink.ignore, simulation)
  }
}
