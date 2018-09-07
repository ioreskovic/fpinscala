import org.scalatest._
import org.scalatest.prop._
import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None
import fpinscala.errorhandling.Option._

import scala.{
  Option => _,
  Some => _,
  Either => _,
  _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


class OptionSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

	"Mapping over Option" when {
		"it has some value" should {
			"yield some value defined by mapping function" in {
				forAll ("original value") { (a: Int) =>
					val f: (Int => Double) = _ + 1
					Some(a).map(f) should be (Some(f(a)))
				}
			}
		}

		"it has no value" should {
			"yield no value" in {
				forAll ("original value") { (a: Int) =>
					val f: (Int => Double) = _ + 1
					None.map(f) should be (None)
				}
			}
		}
	}
}
