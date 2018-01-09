package edu.agh.automata.serializers

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import edu.agh.automata.models._
import spray.json.DefaultJsonProtocol

import scala.util.Try

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import spray.json._

  implicit object AutomataOptionsReader extends RootJsonReader[AutomataOptions] {
    override def read(json: JsValue): AutomataOptions =
      AutomataOptions(
        p = Try(json.asJsObject.fields("p").convertTo[Double]).getOrElse(0.6),
        q = Try(json.asJsObject.fields("q").convertTo[Double]).getOrElse(0.6),
        i = Try(json.asJsObject.fields("i").convertTo[Int]).getOrElse(5),
        sizeX = Try(json.asJsObject.fields("sizeX").convertTo[Int]).getOrElse(10),
        sizeY = Try(json.asJsObject.fields("sizeY").convertTo[Int]).getOrElse(10),
        length = Try(json.asJsObject.fields("length").convertTo[Int]).getOrElse(500),
        initDiseased = Try(json.asJsObject.fields("initDiseased").convertTo[Double]).getOrElse(0.2),
      )
  }

  implicit object StateFormat extends RootJsonFormat[State] {
    override def read(json: JsValue): State = ???

    override def write(obj: State): JsValue = obj match {
      case Healthy => JsString("S")
      case Infected => JsString("I")
      case Immune(_) => JsString("R")
    }
  }

  implicit val changeFormat: RootJsonFormat[StateChange] = jsonFormat3(StateChange.apply)


}