package scalaworkshop.graphics

import cats.Monoid
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.raw.HTMLElement

sealed trait Graphic {
  def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D
}

case class Point(x: Double, y: Double)

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

case class Sprite(source: Point, sprite: HTMLElement, frameWidth: Double, frameHeight: Double, row: Int, column: Int) extends Graphic {
  override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = {
    val r = row.toDouble
    val c = column.toDouble

    ctx.drawImage(image = sprite,
      offsetX = c * frameWidth,
      offsetY = r * frameHeight,
      width = frameWidth,
      height = frameHeight,
      canvasOffsetX = source.x,
      canvasOffsetY = source.y,
      canvasImageWidth = frameWidth,
      canvasImageHeight = frameHeight
    )

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

object Graphic {
  implicit val graphicMonoid: Monoid[Graphic] = new Monoid[Graphic] {
    override def empty: Graphic = new Graphic {
      override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = ctx
    }

    override def combine(x: Graphic, y: Graphic): Graphic = new Graphic {
      override def draw(ctx: CanvasRenderingContext2D): CanvasRenderingContext2D = y.draw(x.draw(ctx))
    }
  }
}