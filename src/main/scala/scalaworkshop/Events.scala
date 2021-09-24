package scalaworkshop

import cats.effect.IO
import scalaworkshop.Action.{actions, forever, setValue, sleep}
import scalaworkshop.Canvas.GameUnit
import scalaworkshop.Game.PointState
import scalaworkshop.Scalaworkshop.Selection
import scalaworkshop.graphics.Point
import Game._
import cats.syntax.semigroup._

object Events {
  def deleteItem[T](game: Game[T], coordinates: (Double, Double)): Game[T] = {
    val (x, y) = coordinates
    val matchingKeys = game.states.filter {
      case (_, PointState(Point(x1, y1))) if x == x1 && y == y1 => true
      case _ => false
    }.keySet

    game.copy(
      states = game.states.filter { case (key, _) => !matchingKeys(key) },
      actions = game.actions.filter { case (key, _) => !matchingKeys(key) }
    )
  }

  def drawSprite(game: Game[State], coordinates: (Double, Double), selection: List[Selection]): Game[State] = selection match {
    case Nil => game
    case l :: ls =>
      val (x, y) = coordinates
      if (ls.forall(_.imgName == l.imgName) && ls.nonEmpty) {
        val colA = (l :: ls).flatMap(smth => List(setValue(smth.col), sleep(0.2)))
        val rowA = (l :: ls).flatMap(smth => List(setValue(smth.row), sleep(0.2)))

        game |+| Game.sprite(
          Game.Cursor(Point(x, y)),
          l.imgName,
          GameUnit,
          GameUnit,
          colActions = List(forever(actions(colA))),
          rowActions = List(forever(actions(rowA))),
          col = l.col,
          row = l.row
        )
      } else {
        game |+| Game.sprite(Game.Cursor(Point(x, y)), l.imgName, GameUnit, GameUnit, col = l.col, row = l.row)
      }
  }
}
