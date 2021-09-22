package scalaworkshop

import cats.Monoid
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.semigroup._
import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D

import scala.util.Random

object Scalaworkshop extends IOApp {
  val Fps = 60
  val Step = 25

  case class Point(x: Double, y: Double)

  // monoid composition
  sealed trait Graphic {
    def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D
  }

  case class Rectangle(source: Point, height: Double, width: Double, color: Option[String], lineWidth: Option[Double]) extends Graphic {
    override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = {
      color.foreach(clr => {
        ctx.strokeStyle = clr
        ctx.fillStyle = clr
      })
      ctx.lineWidth = lineWidth.getOrElse(0)
      ctx.fillRect(source.x, source.y, width, height)
      ctx
    }
  }

  case class Line(source: Point, to: Point, color: Option[String], lineWidth: Option[Double]) extends Graphic {
    override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = {
      color.foreach(ctx.strokeStyle = _)
      lineWidth.foreach(ctx.lineWidth = _)
      ctx.beginPath()
      ctx.moveTo(source.x, source.y)
      ctx.lineTo(to.x, to.y)
      ctx.stroke()
      ctx
    }
  }

  implicit val graphicMonoid: Monoid[Graphic] = new Monoid[Graphic] {
    override def empty: Graphic = new Graphic {
      override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = ctx
    }

    override def combine(x: Graphic, y: Graphic): Graphic = new Graphic {
      override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = y.draw(x.draw(ctx))
    }
  }


  def drawGrid(color: String, height: Double, width: Double, step: Int) = {
    val ws = width / step
    val hs = height / step

    Monoid[Graphic].combineAll(for {
      x <- Range(1, step).toList.map(_.toDouble)
      line <- List(
        Line(Point(x * ws, 0), Point(x * ws, height), Some(color), Some(0.2)),
        Line(Point(0, x * hs), Point(width, x * hs), Some(color), Some(0.2))
      )
    } yield line)
  }

  def run(args: List[String]) = {
    val canvas: dom.html.Canvas = dom.document.getElementById("app").asInstanceOf[dom.html.Canvas]

    val ctx: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val h = canvas.height.toDouble
    val w = canvas.width.toDouble
    val grid = drawGrid("grey", h, w, Step) |+|
      drawGrid("blue", h, w, Step / 5)


    def animationFrame[T](state: T, action: (T, Double) => IO[T], lastFrameAt: Double = 0): IO[T] = for {
      newFrameAt <- IO.async_[Double](cb => {
        dom.window.requestAnimationFrame(delta => cb(Right(delta)))
        ()
      })
      delta = (newFrameAt - lastFrameAt) / 1000
      nextState <- action(state, delta)
      result <- animationFrame(nextState, action, newFrameAt)
    } yield result

    sealed trait Move
    object Move {
      case object Up extends Move

      case object Down extends Move

      case object Left extends Move

      case object Right extends Move
    }

    sealed trait Action {
      def decay(delta: Double): Option[Action]

      def effect(state: Point, delta: Double): Point
    }

    case class Set(point: Point) extends Action {
      override def decay(delta: Double): Option[Action] = None

      override def effect(oldPoint: Point, delta: Double): Point = point
    }
    case class Movement(direction: Move, value: Double) extends Action {
      override def decay(delta: Double): Option[Action] = Some(value - delta)
        .filter(_ > 0)
        .map(Movement(direction, _))

      override def effect(state: Point, delta: Double): Point = direction match {
        case Move.Right => state.copy(x = state.x + Step * delta)
        case Move.Left => state.copy(x = state.x - Step * delta)
        case Move.Up => state.copy(y = state.y + Step * delta)
        case Move.Down => state.copy(y = state.y + Step * delta)
      }
    }

    case class Actions(actions: List[Action]) extends Action {
      override def decay(delta: Double): Option[Action] = {
        val headDecay = actions.headOption.flatMap(_.decay(delta))
          .map(hAction => Actions(hAction :: actions.tail))
        lazy val tailDecay = actions.tail.headOption.map(_ => Actions(actions.tail))

        headDecay.orElse(tailDecay)
      }

      override def effect(state: Point, delta: Double): Point = actions.head.effect(state, delta)
    }

    case class State[T](value: T, actions: Seq[(String, Action)])

    case class Game(elements: Map[String, Point],
                    movements: List[(String, Action)],
                    shapes: List[(String, Point => Graphic)])
    object Game {
      def element(origin: Point,
                  actions: List[Action],
                  element: Point => Graphic): Game = {
        val id = Random.nextString(10)
        Game(Map(id -> origin), actions.map((id, _)), shapes = List(id -> element))
      }

      implicit val GameMonoid: Monoid[Game] = new Monoid[Game] {
        override def empty: Game = Game(Map.empty, List.empty, List.empty)

        override def combine(x: Game, y: Game): Game =
          Game(x.elements ++ y.elements, x.movements ++ y.movements, x.shapes ++ y.shapes)
      }
    }

    val green = Game.element(Point(0, 0), List(Actions(List(Movement(Move.Right, 3), Movement(Move.Down, 3), Movement(Move.Up, 1), Movement(Move.Left, 1), Set(Point(0, 25)), Movement(Move.Right, 2)))), Rectangle(_, Step.toDouble, Step.toDouble, Some("green"), None))
    val red = Game.element(Point(0, 0), List(Movement(Move.Down, 3)), Rectangle(_, Step.toDouble, Step.toDouble, Some("red"), None))
    val yellow = Game.element(Point(0, 0), List(Movement(Move.Down, 3), Movement(Move.Right, 3)), Rectangle(_, Step.toDouble, Step.toDouble, Some("yellow"), None))
    val blue = Game.element(Point(0, 0), List(Movement(Move.Down, 6)), Rectangle(_, Step.toDouble, Step.toDouble, Some("blue"), None))
    val orange = Game.element(Point(0, 0), List(Movement(Move.Down, 6)), Line(_, Point(25, 25), Some("Orange"), Some(3)))

    for {
      _ <- animationFrame[Game](orange combine blue combine green combine red combine yellow, (s: Game, delta) => for {
        _ <- IO(ctx.clearRect(0, 0, w, h))
        _ <- IO(s.shapes.foldLeft(grid) {
          case (grd, (key, graphic)) => grd |+| graphic(s.elements(key))
        }.draw(ctx))
      } yield {
        val a = s.copy(
          s.movements.foldLeft(s.elements)((st, action) => {
            action match {
              case (key, action) => st.updatedWith(key)(_.map(action.effect(_, delta)))
            }
          }),
          s.movements
            .map { case (key, action) => action.decay(delta).map(key -> _) }
            .collect { case Some(v) => v }
        )
        println(a)

        a
      })
    } yield ExitCode.Success

  }
}
