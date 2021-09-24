package scalaworkshop

import cats.effect.{IO, Ref}
import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse._
import org.scalajs.dom
import org.scalajs.dom.html.Image
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.{Event, MouseEvent}
import scalaworkshop.Scalaworkshop.Selection

trait Canvas[F[_]] {
  def getElement(id: String): F[HTMLElement]

  def createElement(tag: String): F[HTMLElement]

  def getNextAnimationFrame: F[Double]
}

object Canvas {
  val Step: Int = 16
  val GameUnit: Int = Step * 2
  val SecondInMs = 1000

  def js[F[_] : Async]: Canvas[F] = new Canvas[F] {
    override def getElement(id: String): F[HTMLElement] = Async[F].delay {
      dom.document.getElementById(id).asInstanceOf[HTMLElement]
    }

    override def createElement(tag: String): F[HTMLElement] = Async[F].delay {
      dom.document.createElement(tag).asInstanceOf[HTMLElement]
    }

    override def getNextAnimationFrame: F[Double] = Async[F].async_[Double](cb => {
      dom.window.requestAnimationFrame(delta => cb(Right(delta)))
      ()
    })
  }

  def animationFrame[T](canvas: Canvas[IO], state: T, action: (T, Double) => IO[T], lastFrameAt: Option[Double] = None): IO[T] = for {
    newFrameAt <- canvas.getNextAnimationFrame
    result <- lastFrameAt.map(fAt => (newFrameAt - fAt) / SecondInMs) match {
      case Some(delta) => for {
        newState <- action(state, delta)
        res <- animationFrame(canvas, newState, action, Some(newFrameAt))
      } yield res
      case None => animationFrame(canvas, state, action, Some(newFrameAt))
    }
  } yield result

  def addEventListener[T, H <: HTMLElement, I <: dom.Event](element: H, `type`: String, fn: I => IO[T])(implicit runtime: IORuntime): IO[IO[Option[T]]] = for {
    queue <- Queue.circularBuffer[IO, T](1)
    _ <- IO(element.addEventListener(
      `type`,
      (e: I) => fn(e).flatMap(res => queue.offer(res).map(_ => res)).attempt.unsafeRunAndForget()
    ))
  } yield queue.tryTake

  def addPersistedEventListener[T, H <: HTMLElement, I <: dom.Event](element: H, onType: String, offType: String, fn: I => IO[T])(implicit runtime: IORuntime): IO[IO[Option[T]]] = for {
    ref <- Ref.of[IO, Option[T]](None)
    _ <- IO(element.addEventListener(
      onType,
      (e: I) => fn(e).flatMap(res => ref.set(Some(res)).map(_ => res)).attempt.unsafeRunAndForget()
    ))
    _ <- IO(element.addEventListener(
      offType,
      (e: I) => fn(e).flatMap(res => ref.set(None).map(_ => res)).attempt.unsafeRunAndForget()
    ))
  } yield ref.get

  def loadImages(canvas: Canvas[IO], images: Map[String, String]): IO[Map[String, HTMLElement]] = images.toList.map {
    case (key, url) =>
      for {
        elm <- canvas.createElement("img")
        res <- IO.async_[(String, HTMLElement)](cb => {
          val img = elm.asInstanceOf[Image]
          img.src = url

          if (img.complete) {
            cb(Right((key, elm)))
          } else {
            img.onload = (_: Event) => cb(Right((key, elm)))
            elm.addEventListener("error", (_: Event) => cb(Left(new RuntimeException(s"Failed to load $key"))))
          }
        })
      } yield res
  }.sequence.map(_.toMap)

  def prepareMenu(canvas: Canvas[IO], images: Map[String, Image], addToState: Selection => IO[Unit])(implicit runtime: IORuntime): IO[Unit] = for {
    menu <- canvas.getElement("menu")
    _ <- images.flatMap {
      case (name, img) => Range(0, img.height / GameUnit).flatMap(x => Range(0, img.width / GameUnit).map((x, _))).map {
        case (col, row) => for {
          elm <- canvas.createElement("div")
          _ <- {
            elm.className = "sprite"
            elm.style = s"background-position: -${row * GameUnit}px -${col * GameUnit}px; background-image: url('${img.src}')"

            IO(menu.appendChild(elm))
          }
          _ <- IO {
            elm.addEventListener("click", (_: MouseEvent) => addToState(Selection(name, row, col)).unsafeRunAndForget())
          }
        } yield ()
      }
    }.toList.sequence
  } yield ()

  def drawQueue(canvas: Canvas[IO], images: Map[String, Image], values: List[Selection]): IO[Unit] = for {
    review <- canvas.getElement("review")
    elms <- values.map(select => canvas.createElement("div").map { elm =>
      elm.className = "sprite"
      elm.style = s"background-position: -${select.row * GameUnit}px -${select.col * GameUnit}px; background-image: url('${images(select.imgName).src}')"
      elm
    }).sequence
    _ <- IO(review.innerHTML = "")
    _ <- elms.map(elm => IO(review.appendChild(elm))).sequence
  } yield ()

  def getMousePosition(canvas: dom.html.Canvas, evt: MouseEvent): IO[(Double, Double)] = IO.delay(canvas.getBoundingClientRect()).map(rect => {
    val scaleX = canvas.width / rect.width
    val scaleY = canvas.height / rect.height

    val x = Math.floor((evt.clientX - rect.left) * scaleX / GameUnit) * GameUnit
    val y = Math.floor((evt.clientY - rect.top) * scaleY / GameUnit) * GameUnit

    (x, y)
  })
}
