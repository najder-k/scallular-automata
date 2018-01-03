package edu.agh.automata.models

import org.scalacheck.Gen

import scala.util.Random

class AutomataState(sizeX: Int = 100,
                    sizeY: Int = 100,
                    p: Double = 0.6,
                    q: Double = 0.9,
                    immunity: Int = 3,
                    initDiseased: Double = 0.01) {

  def prettyPrint(state: Array[Array[State]]): String = {
    state.map(_.map(simpleShow).mkString).mkString("\n")
  }

/*  private def show(state: State): String = state match {
    case Healthy   => "H"
    case Infected  => "X"
    case Immune(x) => s"O$x"
  }*/

  private def simpleShow(state: State): String = state match {
    case Healthy   => "H"
    case Infected  => "X"
    case Immune(_) => "O"
  }

  def init(): Array[Array[State]] = {
    Array.fill(sizeX, sizeY)(stateGen.sample.get)
  }

  val stateGen: Gen[State] = {
    val healthy = (1 / initDiseased).toInt
    Gen.frequency(
      (healthy - 1, Healthy),
      (1, Infected)
    )
  }

  def json(stuff: Seq[StateChange]): String = {
    stuff.map { case StateChange(x, y, state) =>
      s"""{"x": $x, "y": $y, "state": "${simpleShow(state)}"}"""
    }.mkString("[", ",", "]")
  }

  def json(stuff: Array[Array[State]]): String = {
    val all = stuff.zipWithIndex.flatMap { case (ys, x) =>
      ys.zipWithIndex.map { case (cell, y) =>
        StateChange(x, y, cell)
      }
    }.toSeq
    json(all)
  }

  def step(state: Array[Array[State]]): Array[Array[State]] = {
    state.zipWithIndex.map { case (ys, x) =>
      ys.zipWithIndex.map { case (cell, y) =>
        checkChange(cell, neighbours(x, y, state))
      }
    }
  }

  def delta(stateBefore: Array[Array[State]], stateAfter: Array[Array[State]]): Seq[StateChange] = {
    (stateBefore zip stateAfter zipWithIndex) flatMap { case ((lineBefore, lineAfter), x) =>
      (lineBefore zip lineAfter zipWithIndex) collect { case ((stateOld, stateNew), y) if stateOld != stateNew =>
        StateChange(x, y, stateNew)
      }
    }
  }

  def deltaStep(state: Array[Array[State]]): Seq[StateChange] = {
    state.zipWithIndex.flatMap { case (ys, x) =>
      ys.zipWithIndex.map { case (cell, y) =>
        checkChangeDelta(cell, neighbours(x, y, state)).map(state => StateChange(x, y, state))
      }
    }.collect { case Some(cell) => cell }
  }

  def deltaStepSimple(state: Array[Array[State]]): Seq[StateChange] = {
    deltaStep(state).filter {
      case StateChange(_, _, Immune(n)) if n != immunity => false
      case _ => true
    }
  }

  def isDead(state: Array[Array[State]]): Boolean = {
    state.flatten.forall {
      case Healthy => true
      case _ => false
    }
  }

  private def neighbours(x: Int, y: Int, state: Array[Array[State]]): Seq[State] = {
    //todo: also take neighbours from adjecent parts
    val fs: Seq[Int => Int] = Seq(_ - 1, identity, _ + 1)
    val xs = fs.map(_(x))
    val ys = fs.map(_(y))
    (xs cross ys)
      .filter { _ != ((x, y)) }
      .filter { case (nX, nY) => (0 until sizeX contains nX) && (0 until sizeY contains nY) }
      .map { case (nX, nY) => state(nX)(nY) }
  }

  private def checkChange(state: State, neighbours: Seq[State]): State =
    checkChangeDelta(state, neighbours).getOrElse(state)

  private def checkChangeDelta(state: State, neighbours: Seq[State]): Option[State] = state match {
    case Healthy =>
      val r = neighbours.count(_ == Infected)
      val chance = 1 - math.pow(1 - q, r.toDouble)
      if (Random.nextDouble() < chance) Some(Infected)
      else None

    case Infected =>
      val chance = p
      if (Random.nextDouble() > chance) Some(Immune(immunity))
      else None

    case Immune(0) =>
      Some(Healthy)

    case Immune(n) =>
      Some(Immune(n - 1))
  }

  implicit class Cross[A](as: Seq[A]) {
    def cross[B](bs: Seq[A]): Seq[(A, A)] = for {
      a <- as
      b <- bs
    } yield (a, b)
  }
}



sealed trait State
case object Healthy extends State
case object Infected extends State
final case class Immune(n: Int) extends State

case class StateChange(x: Int, y: Int, state: State)