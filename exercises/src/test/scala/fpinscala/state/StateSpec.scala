import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.state.RNG
import fpinscala.state.RNG._

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
}
