package fpinscala.state

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  private def become =
    (i: Input) =>
      (m: CandyMachine) =>
        (m, i) match {
          case (CandyMachine(_, 0, _), _) => m
          case (CandyMachine(true, candies, coins), Coin) =>
            CandyMachine(false, candies, coins + 1)
          case (CandyMachine(false, candies, coins), Turn) =>
            CandyMachine(true, candies - 1, coins)
          case _ => m
    }

  def stateChanges(inputs: List[Input]): List[State[CandyMachine, Unit]] =
    inputs.map(i => State.modify[CandyMachine](become(i)))

  def combined(stateTransformations: List[State[CandyMachine, Unit]])
    : State[CandyMachine, List[Unit]] =
    State.sequence(stateTransformations)

  def simulateMachine[R](inputs: List[Input])(
      f: CandyMachine => R): State[CandyMachine, R] =
    for {
      ignoredAction <- combined(stateChanges(inputs))
      currState     <- State.get
    } yield f(currState)
}
