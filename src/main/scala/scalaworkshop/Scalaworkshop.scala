package scalaworkshop

import cats.Monoid
import cats.effect.unsafe.IORuntime
import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits._
import org.scalajs.dom
import org.scalajs.dom.html.Image
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{KeyboardEvent, MouseEvent}
import scalaworkshop.Canvas.{getMousePosition, _}
import scalaworkshop.Game._
import scalaworkshop.graphics._

object Scalaworkshop extends IOApp {
  implicit lazy val rt: IORuntime = runtime

  case class Selection(imgName: String, row: Int, col: Int)

  def run(args: List[String]): IO[ExitCode] = {
    val imageConfig = Map(
      "people" -> "/img/people.png",
      "game" -> "/img/game.png",
      "forest" -> "/img/forest.png",
      "water" -> "/img/water.png"
    )

    val jsCanvas = Canvas.js[IO]

    for {
      canvas <- jsCanvas.getElement("app").map(_.asInstanceOf[dom.html.Canvas])
      ctx <- IO(canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D])
      canvasHeight = canvas.height.toDouble
      canvasWidth = canvas.width.toDouble
      grid = Shapes.grid("grey", canvasHeight, canvasWidth, Step)
      _ <- IO(grid.draw(ctx))
      selection <- Ref.of[IO, List[Selection]](List.empty)
      mouseClick <- Canvas.addEventListener(canvas, "click", getMousePosition(canvas, (_: MouseEvent)))
      mouseDbClick <- Canvas.addEventListener(canvas, "dblclick", getMousePosition(canvas, (_: MouseEvent)))
      keyboardClick <- Canvas.addPersistedEventListener(dom.document.asInstanceOf[HTMLElement], "keydown", "keyup", (e: KeyboardEvent) => IO.pure(e.key))
      images <- loadImages(jsCanvas, imageConfig)
      imgElms = images.map { case (key, elm) => key -> elm.asInstanceOf[Image] }
      _ <- prepareMenu(jsCanvas, imgElms, select => {
        selection.update(select :: _) *> selection.get.flatMap(drawQueue(jsCanvas, imgElms, _))
      })
      _ <- animationFrame[Game[State]](jsCanvas, Monoid.empty[Game[State]], (state: Game[State], delta) => for {
        s1 <- mouseDbClick.map(_.map(Events.deleteItem(state, _)).getOrElse(state))
        s <- keyboardClick.map2(mouseClick)((_, _)).flatMap {
          case (Some(key), Some(coordinates)) if key == "Control" => selection.get.map(Events.drawSprite(s1, coordinates, _))
          case (_, Some(coordinates)) => for {
            processState <- selection.get.map(Events.drawSprite(s1, coordinates, _))
            _ <- selection.set(List.empty) *> drawQueue(jsCanvas, imgElms, List.empty)
          } yield processState
          case _ => IO.pure(s1)
        }
        _ <- IO(ctx.clearRect(0, 0, canvasWidth, canvasHeight))
        _ <- IO(s.shapes.foldLeft(grid) {
          case (grd, graphic) => grd |+| graphic(s.states, images)
        }.draw(ctx))
      } yield {
        s.copy(
          s.actions.foldLeft(s.states)((st, action) => action match {
            case (key, action) => st.updatedWith(key)(_.flatMap(action.effect(_, delta)))
          }),
          s.actions
            .map { case (key, action) => action.decay(delta).map(key -> _) }
            .collect { case Some(v) => v }
        )
      })
    } yield ExitCode.Success
  }
}
