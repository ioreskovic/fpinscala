import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Left
import fpinscala.errorhandling.Right
import fpinscala.errorhandling.Either._
import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
}

class EitherSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

	"Mapping over Either" when {
		"it is Left" should {
			"yield Left with same error" in {
				val left = Left("Error")
				left.map(identity) should be (Left("Error"))
			}
		}

		"it is Right" should {
			"yield Right with transformed value" in {
				val x = 3
				val right = Right(x)
				val f: (Int => Double) = _ + 1.0

				right.map(f) should be (Right(f(x)))
			}
		}
	}

	"FlatMapping over Either" when {
		"it is Left" should {
			"yield Left with same error" in {
				val left: Either[String, Int] = Left("Error")
				left.flatMap(i => Right(i + 1.0)) should be (Left("Error"))
			}
		}

		"it is Right" should {
			"yield Right with transformed value" in {
				val x = 1
				val right: Either[String, Int] = Right(x)
				val f: (Int => Double) = _ + 1.0

				right.flatMap(i => Right(f(i))) should be (Right(f(x)))
			}

			"yield Left with error" in {
				val right: Either[String, Int] = Right(1)

				right.flatMap(i => Left("Ooops!!")) should be (Left("Ooops!!"))
			}
		}
	}

	"Falling back on Either" when {
		"it is left" should {
			"yield fallback Either" in {
				val left: Either[String, Int] = Left("Error")
				val fallback: Either[String, Int] = Right(42)

				left.orElse(fallback) should be (fallback)
			}
		}

		"it is right" should {
			"yield original right" in {
				val right: Either[String, Int] = Right(42)
				val fallback: Either[String, Int] = Left("Error")

				right.orElse(fallback) should be (right)
			}
		}
	}

	"Combining 2 Eithers" when {
		"Both are Left" should {
			"yield Left" in {
				val e1: Either[String, Int] = Left("ErrorA")
				val e2: Either[String, Double] = Left("ErrorB")
				val f: (Int, Double) => Double = _ + _

				e1.map2(e2)(f) should be (Left("ErrorA"))
			}
		}

		"First is Left only" should {
			"yield Left" in {
				val e1: Either[String, Int] = Left("ErrorA")
				val e2: Either[String, Double] = Right(42.0)
				val f: (Int, Double) => Double = _ + _

				e1.map2(e2)(f) should be (Left("ErrorA"))
			}
		}

		"Second is Left only" should {
			"yield Left" in {
				val e1: Either[String, Int] = Right(42)
				val e2: Either[String, Double] = Left("ErrorB")
				val f: (Int, Double) => Double = _ + _

				e1.map2(e2)(f) should be (Left("ErrorB"))
			}
		}

		"Both are Right" should {
			"yield Right of composed value" in {
				val i = 42
				val d = 42.0
				val e1: Either[String, Int] = Right(i)
				val e2: Either[String, Double] = Right(d)
				val f: (Int, Double) => Double = _ + _

				e1.map2(e2)(f) should be (Right(f(i, d)))
			}
		}
	}
}