package scalaworkshop

import cats.Monoid
import org.scalajs.dom.raw.HTMLElement
import scalaworkshop.graphics.{Graphic, Line, Point, Rectangle, Sprite}
import scala.util.Random
import cats.syntax.contravariantSemigroupal._
import Canvas.Step

case class Game[T](states: Map[String, T],
                   actions: List[(String, Action)],
                   shapes: List[(Map[String, T], Map[String, HTMLElement]) => Graphic])

object Game {
  case class Cursor(point: Point, actions: List[Action] = List.empty)

  sealed trait State

  case class PointState(point: Point) extends State

  case class ValueState(value: Double) extends State

  object PointState {
    def getPoint(state: State): Option[Point] = state match {
      case s@PointState(_) => Some(s.point)
      case _ => None
    }
  }

  object ValueState {
    def getValue(state: State): Option[Double] = state match {
      case ValueState(value) => Some(value)
      case _ => None
    }
  }

  def genId = Random.nextString(10)

  def line(x: Cursor, y: Cursor, color: Option[String] = None, lineWidth: Option[Double] = None): Game[State] = {
    val xid = genId
    val yid = genId

    Game(Map(xid -> PointState(x.point), yid -> PointState(y.point)), x.actions.map((xid, _)) ++ y.actions.map((yid, _)), shapes = List((fullState: Map[String, State], _: Map[String, HTMLElement]) => {
      val x = fullState.get(xid).flatMap(PointState.getPoint)
      val y = fullState.get(yid).flatMap(PointState.getPoint)

      (x, y).mapN(Line(_, _, color, lineWidth)).getOrElse(Monoid.empty[Graphic])
    }))
  }

  def rect(x: Cursor, height: Int, width: Int, heightActions: List[Action] = List.empty, widthActions: List[Action] = List.empty, color: Option[String] = None, lineWidth: Option[Double] = None): Game[State] = {
    val xid = genId
    val hid = genId
    val wid = genId

    val actions = x.actions.map((xid, _)) ++ heightActions.map((hid, _)) ++ widthActions.map((wid, _))

    Game(Map(
      xid -> PointState(x.point),
      hid -> ValueState(height.toDouble * Step),
      wid -> ValueState(width.toDouble * Step)
    ), actions, shapes = List((fullState: Map[String, State], _: Map[String, HTMLElement]) => {
      val x = fullState.get(xid).flatMap(PointState.getPoint)
      val h = fullState.get(hid).flatMap(ValueState.getValue)
      val w = fullState.get(wid).flatMap(ValueState.getValue)

      (x, h, w).mapN(Rectangle(_, _, _, color, lineWidth)).getOrElse(Monoid.empty[Graphic])
    }))
  }

  def sprite(x: Cursor, spriteKey: String, width: Int, height: Int, colActions: List[Action] = List.empty, rowActions: List[Action] = List.empty, col: Int = 0, row: Int = 0): Game[State] = {
    val xid = genId
    val cid = genId
    val rid = genId

    val actions = x.actions.map((xid, _)) ++ colActions.map((cid, _)) ++ rowActions.map((rid, _))

    Game(Map[String, State](
      xid -> PointState(x.point),
      cid -> ValueState(col.toDouble),
      rid -> ValueState(row.toDouble)
    ), actions, shapes = List((fullState: Map[String, State], images: Map[String, HTMLElement]) => {
      val x = fullState.get(xid).flatMap(PointState.getPoint)
      val c = fullState.get(cid).flatMap(ValueState.getValue).map(_.toInt)
      val r = fullState.get(rid).flatMap(ValueState.getValue).map(_.toInt)

      (x, c, r).mapN((x, c, r) => Sprite(x, images(spriteKey), width.toDouble, height.toDouble, c, r)).getOrElse(Monoid.empty[Graphic])
    }))
  }

  implicit def GameMonoid[T]: Monoid[Game[T]] = new Monoid[Game[T]] {
    override def empty: Game[T] = Game(Map.empty, List.empty, List.empty)

    override def combine(x: Game[T], y: Game[T]): Game[T] =
      Game(x.states ++ y.states, x.actions ++ y.actions, x.shapes ++ y.shapes)
  }
}
