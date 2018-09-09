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
}
