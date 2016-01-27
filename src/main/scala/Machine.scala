import State._
import State.{modify, get, set}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def update = (input: Input) => (machine: Machine) =>
    (input, machine) match{
      case (_, Machine(_, 0, _))        => machine
      case (Turn, Machine(true, _, _)) => machine
      case (Coin, Machine(false, _, _))  => machine
      case (Turn, Machine(false, ca, c)) => Machine(true, ca - 1, c)
      case (Coin, Machine(true, ca, c)) if(machine.candies > 0) =>
        Machine(false, ca, c + 1)
      case (_, _) => machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

}
