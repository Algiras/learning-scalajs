package scalaworkshop

import scalaworkshop.Canvas.Step
import scalaworkshop.Game.{PointState, ValueState}
import scalaworkshop.graphics.Point

sealed trait Action {
  def decay(delta: Double): Option[Action]

  def effect(state: Game.State, delta: Double): Option[Game.State]
}

object Action {

  sealed trait Move

  object Move {
    case object Up extends Move

    case object Down extends Move

    case object Left extends Move

    case object Right extends Move
  }

  def setValue(setValue: Double): Action = new Action {
    override def decay(delta: Double): Option[Action] = None

    override def effect(value: Game.State, delta: Double): Option[Game.State] = value match {
      case ValueState(_) => Some(ValueState(setValue))
      case PointState(_) => None
    }
  }

  def setPoint(point: Point): Action = new Action {
    override def decay(delta: Double): Option[Action] = None

    override def effect(value: Game.State, delta: Double): Option[Game.State] = value match {
      case PointState(_) => Some(PointState(point))
      case _ => None
    }
  }

  def forever(action: Action, cache: Option[Action] = None): Action = new Action {
    override def decay(delta: Double): Option[Action] = (cache, action.decay(delta)) match {
      case (Some(_), Some(action)) => Some(forever(action, cache))
      case (None, Some(decayedAction)) => Some(forever(decayedAction, Some(action)))
      case (Some(preservedAction), None) => Some(forever(preservedAction, Some(preservedAction)))
      case (None, None) => Some(forever(action, Some(action)))
    }

    override def effect(state: Game.State, delta: Double): Option[Game.State] = action.effect(state, delta)
  }

  def update(value: Double, velocity: Option[Double] = None): Action = new Action {
    private val internalVelocity = velocity.getOrElse(1.0)

    override def decay(delta: Double): Option[Action] = Some(value - (delta * internalVelocity))
      .filter(_ > 0)
      .map(v => update(v, velocity))

    override def effect(state: Game.State, delta: Double): Option[Game.State] = state match {
      case ValueState(value) => Some(ValueState(value + Step * internalVelocity * delta))
      case _ => None
    }
  }

  def sleep(duration: Double): Action = new Action {
    override def decay(delta: Double): Option[Action] = Some(duration - delta)
      .filter(_ > 0)
      .map(sleep)

    override def effect(state: Game.State, delta: Double): Option[Game.State] = Some(state)
  }

  def movement(direction: Move, value: Double, velocity: Option[Double] = None): Action = new Action {
    private val internalVelocity = velocity.getOrElse(1.0)

    override def decay(delta: Double): Option[Action] = Some(value - (delta * internalVelocity))
      .filter(_ > 0)
      .map(v => movement(direction, v, velocity))

    override def effect(value: Game.State, delta: Double): Option[Game.State] = value match {
      case PointState(point) =>
        val stepModifier = Step * internalVelocity * delta

        Some(PointState(direction match {
          case Move.Right => point.copy(x = point.x + stepModifier)
          case Move.Left => point.copy(x = point.x - stepModifier)
          case Move.Up => point.copy(y = point.y + stepModifier)
          case Move.Down => point.copy(y = point.y + stepModifier)
        }))
      case _ => None
    }
  }

  def actions(actionList: List[Action]): Action = new Action {
    override def decay(delta: Double): Option[Action] = {
      val headDecay = actionList.headOption.flatMap(_.decay(delta))
        .map(hAction => actions(hAction :: actionList.tail))
      lazy val tailDecay = actionList.tail.headOption.map(_ => actions(actionList.tail))

      headDecay.orElse(tailDecay)
    }

    override def effect(state: Game.State, delta: Double): Option[Game.State] = actionList.headOption.flatMap(_.effect(state, delta))
  }
}
