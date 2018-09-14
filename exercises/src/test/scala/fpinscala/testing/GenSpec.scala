package fpinscala.testing

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{ Gen => SCGen }

class GenSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

	"summing a list" when {
		"it is empty" should {
			"yield zero" in {
				List[Int]().sum should be (0)
			}
		}
	}

	"summing a list" when {
		"it has only one element" should {
			"yield that element" in {
				forAll (SCGen.listOfN(1, SCGen.choose(-100, 100))) { (l: List[Int]) =>
					l.sum should be (l.head)
				}
			}
		}
	}

	"summing a list" when {
		"it has all elements equal" should {
			"yield len * elem" in {
				forAll (SCGen.choose(10, 100), SCGen.choose(-100, 100)) { (l: Int, e: Int) =>
					List.fill(l)(e).sum should be (l * e)
				}
			}
		}
	}

	"sum of a list" should {
		"be equal to sum of it's tail and head" in {
			forAll (SCGen.listOfN(33, SCGen.choose(-100, 100))) { (l: List[Int]) =>
				l.sum should be (l.head + l.tail.sum)
			}
		}
	}
}