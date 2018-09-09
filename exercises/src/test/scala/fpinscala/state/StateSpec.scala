import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.state.RNG
import fpinscala.state.RNG._
import fpinscala.state.State
import fpinscala.state.Machine
import fpinscala.state.Input
import fpinscala.state.Coin
import fpinscala.state.Turn
import fpinscala.state.Machine._
import fpinscala.state.CandyMachine
import fpinscala.state.CandyMachine._

class StateSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {
	"Generating non-negative int" should {
		"produce non negative numbers only" in {
			forAll { (seed: Long) =>
				RNG.nonNegativeInt(Simple(seed))._1 should be >= 0
			}
		}
	}

	"Generating random double" should {
		"produce number in range [0.0, 1.0> only" in {
			forAll { (seed: Long) =>
				val result = RNG.double(Simple(seed))._1

				result should be >= 0.0
				result should be < 1.0
			}
		}
	}

	"Generating list of random ints" should {
		"produce required number of elements" in {
			val n = 10
			val seedRng = Simple(0L)
			RNG.ints(n)(seedRng)._1.length should be (n)
		}
	}

	"Sequencing rands" should {
		"produce rand of a sequence" in {
			val r = Simple(0L)

			RNG.sequence(List(unit(1), unit(2), unit(3)))(r)._1 should be (List(1, 2, 3))
		}
	}

	"Inserting a coin" when {
		"Machine is unlocked and has candies" should {
			"Lock the machine and take the coin" in {
				State.simulateMachine(List(Coin)).run(Machine(true, 1, 0))._2 should be (Machine(false, 1, 1))
			}
		}

		"Machine is unlocked and has no candies" should {
			"Do nothing" in {
				val initialState = Machine(true, 0, 0)
				State.simulateMachine(List(Coin)).run(initialState)._2 should be (initialState)
			}
		}

		"Machine is locked (ignoring candies)" should {
			"Do nothing" in {
				val initialState = Machine(false, 1, 0)
				State.simulateMachine(List(Coin)).run(initialState)._2 should be (initialState)
			}
		}
	}

	"Turning the knob" when {
		"Machine is locked and has candies" should {
			"Dispense candy and unlock machine" in {
				val initialState = Machine(false, 1, 1)

				State.simulateMachine(List(Turn)).run(initialState)._2 should be (Machine(true, initialState.candies - 1, initialState.coins))
			}
		}

		"Machine is locked and has no candies" should {
			"Do nothing" in {
				val initialState = Machine(false, 0, 1)

				State.simulateMachine(List(Turn)).run(initialState)._2 should be (initialState)
			}
		}

		"Machine is unlocked (ignoring candies)" should {
			"Do nothing" in {
				val initialState = Machine(true, 1, 1)

				State.simulateMachine(List(Turn)).run(initialState)._2 should be (initialState)
			}
		}
	}

	"Feeding valid inputs to machine" should {
		"Leave machine in correct state" in {
			val initialState = CandyMachine(true, 5, 10)
			val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)

			val (candies, coins) = CandyMachine.simulateMachine(inputs)(m => 
				(m.candies, m.coins)).run(initialState)._1

			candies should be (initialState.candies - 4)
			coins should be (initialState.coins + 4)
		}
	}
}
