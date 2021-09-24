package scalaworkshop.graphics

import cats.Monoid
import cats.syntax.semigroup._

object Shapes {
  def grid(color: String, height: Double, width: Double, step: Int): Graphic = {
    val ratio = Math.round(width / height).toInt

    val ws = width / step
    val hs = height / step

    val vertical = Monoid[Graphic].combineAll(for {
      x <- Range(1, step * ratio).toList.map(_.toDouble)
      line <- List(
        Line(Point(x * ws / ratio, 0), Point(x * ws / ratio, height), Some(color), Some(0.2)),
      )
    } yield line)

    val horizontal = Monoid[Graphic].combineAll(for {
      x <- Range(1, step).toList.map(_.toDouble)
      line <- List(
        Line(Point(0, x * hs), Point(width, x * hs), Some(color), Some(0.2))
      )
    } yield line)

    vertical |+| horizontal
  }
}
