package fpinscala.testing

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.{ Gen => SCGen }
import fpinscala.testing.Gen._
import fpinscala.parallelism.Par2
import fpinscala.parallelism.Par2._

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

	"max of an empty list" should {
		"throw exception" in {
			assertThrows[UnsupportedOperationException] {
				List[Int]().max
			}
		}
	}

	"max of a singleton list" should {
		"match it's only element, head" in {
			forAll (SCGen.listOfN(1, SCGen.choose(-100, 100))) { (l: List[Int]) =>
				l.max should be (l.head)
			}
		}
	}

	"max of a list" should {
		"yield the biggest element" in {
			forAll (SCGen.listOfN(33, SCGen.choose(-100, 100))) { (l: List[Int]) =>
				l.max should be (l.sortBy(- _).head)
			}
		}
	}

  "checking max list property" when {
    "using empty list generators" should {
      "yield falsified" in {
        val smallInt = Gen.choose(-10, 10)
        val maxProp = Prop.forAll(Gen.listOf(smallInt)) { l =>
          val max = l.max
          !l.exists(_ > max) // No value greater than `max` should exist in `l`
        }

        Prop.check(maxProp).isFalsified should be (true)
      }
    }

    "using non-empty list generators" should {
      "yield passed" in {
        val smallInt = Gen.choose(-10, 10)
        val maxProp1 = Prop.forAll(Gen.listOf1(smallInt)) { l =>
          val max = l.max
          !l.exists(_ > max) // No value greater than `max` should exist in `l`
        }

        // Prop.check(maxProp1).isFalsified should be (false)
      }
    }
  }

  "checking List#sorted property" should {
    "pass" in {
      val smallInt = Gen.choose(-10, 10)

      val sortedProp = Prop.forAll(Gen.listOf(smallInt)) { list =>
        val sortedList = list.sorted

        val empty = sortedList.isEmpty
        val singleton = sortedList.length == 1

        // bah, cant use List#sliding to get a Tuple2 here, a shame
        val orderedElems = !sortedList.zip(sortedList).exists {
          case (a, b) => a > b
        }

        val outputHasAllInput = !list.exists(!sortedList.contains(_))
        val outputDoesNotHaveNonInput = !sortedList.exists(!list.contains(_))

        (empty || singleton || orderedElems) && outputHasAllInput && outputDoesNotHaveNonInput
      }

      Prop.check(sortedProp).isFalsified should be (false)
    }
  }

  "checking fork property" should {
    "pass" in {
      val genValue = Gen.choose(-10, 10)
      val lenValue = Gen.choose(0, 20)

      val forkProp = Prop.forAllPar(Gen.pint2(genValue, lenValue))(i => 
        Gen.equal(Par2.fork(i), i)
      )

      // Prop.check(forkProp).isFalsified should be (false)
    }
  }

  /**
	"checking combined two props" when {
		"both check OK" should {
			"yield true" in {
				val ok1 = new Prop {
					override def check: Boolean = true
				}

				val ok2 = new Prop {
					override def check: Boolean = true
				}

				(ok1 && ok2).check should be (true)
			}
		}

		"at least one checks false" should {
			"yield false" in {
				val ok = new Prop {
					override def check: Boolean = true
				}

				val notOk = new Prop {
					override def check: Boolean = false
				}

				(ok && notOk).check should be (false)
				(notOk && ok).check should be (false)
			}
		}
	}
  */
}
