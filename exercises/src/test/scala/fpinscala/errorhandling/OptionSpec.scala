import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
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
				val f: (Int => Double) = _ + 1

				forAll ("original value") { (a: Int) =>
					Some(a).map(f) should be (Some(f(a)))
				}
			}
		}

		"it has no value" should {
			"yield no value" in {
				val f: (Int => Double) = _ + 1

				forAll ("original value") { (a: Int) =>
					None.map(f) should be (None)
				}
			}
		}
	}

	"Extracting Option value will fallback value" when {
		"it has some value" should {
			"yield some value from option" in {
				val x = 1
				val fallback = 0

				Some(x).getOrElse(fallback) should be (x)
			}
		}

		"it has no value" should {
			"yield fallback value" in {
				val fallback = 0
				
				None.getOrElse(fallback) should be (fallback)
			}
		}
	}

	"Flatmapping over Option" when {
		"it has some value" should {
			"yield option from arg function" in {
				val f: (Int => Option[Double]) = i => Some(i + 1)
				val x = 1

				Some(x).flatMap(f) should be (f(x))
			}
		}

		"it has no value" should {
			"yield none" in {
				val f: (Int => Option[Double]) = i => Some(i + 1)
				
				None.flatMap(f) should be (None)
			}
		}
	}

	"Falling back over Option" when {
		"it has some value" should {
			"yield original Some" in {
				val x = 1
				Some(x).orElse(Some(x + 1)) should be (Some(x))
			}
		}

		"it has no value" should {
			"yield fallback option" in {
				val x = 1
				None.orElse(Some(x)) should be (Some(x))
			}
		}
	}

	"Filtering over Option" when {
		"it has some value" when {
			"it matches predicate" should {
				"yield that some value" in {
					val x = 5
					Some(x).filter(_ => true) should be (Some(x))
				}
			}

			"it doesn't match predicate" should {
				"yield none" in {
					val x = 5
					Some(x).filter(_ => false) should be (None)
				}
			}
		}

		"it has no value" should {
			"yield none" in {
				None.filter(_ => true) should be (None)
			}
		}
	}

	"Calculating variance" when {
		"Sequence is empty" should {
			"yield None" in {
				Option.variance(Seq.empty) should be (None)
			}
		}

		"Sequence is not empty" should {
			"yield Some result" in {
				Option.variance(Seq(1, 2, 3, 4, 5)) should be (Some(2.0))
			}
		} 
	}

	"Combining 2 options" when {
		"both contain no value" should {
			"yield none" in {
				Option.map2(None: Option[Int], None: Option[Int])(_ + _) should be (None)
			}
		}

		"only first contains no value" should {
			"yield none" in {
				Option.map2(None: Option[Int], Some(1))(_ + _) should be (None)
			}
		}

		"both contain a value" should {
			"yield some result of function" in {
				val f: (Int, Int) => Int = _ + _
				val a = 1
				val b = 2

				Option.map2(Some(a), Some(b))(f) should be (Some(f(a, b)))
			}
		}
	}

	"Sequencing list of options" when {
		"it is empty" should {
			"yield some empty list" in {
				Option.sequence[Any](Nil) should be (Some(Nil))
			}
		}

		"it has only nones" should {
			"yield none" in {
				Option.sequence[Any](List(None, None, None)) should be (None)
			}
		}

		"it has only somes" should {
			"yield some list" in {
				val original = List(1, 2, 3)

				Option.sequence(original.map(Some(_))) should be (Some(original))
			}
		}

		"it has none element" should {
			"yield none" in {
				Option.sequence(List(Some(1), Some(2), None, Some(3), Some(4))) should be (None)
			}
		}
	}
}
