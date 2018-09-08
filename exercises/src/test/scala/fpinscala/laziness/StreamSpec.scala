import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.laziness.Stream
import fpinscala.laziness.Stream._

class StreamSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

	"Transforming Stream to List" when {
		"it is empty" should {
			"yield empty list" in {
				Stream.empty.toList should be (List.empty)
			}
		}

		"it is not empty" should {
			"yield a list with elements in original order" in {
				Stream(1, 2, 3).toList should be (List(1, 2, 3))
			}
		}
	}

	"Taking n elements from Stream" when {
		"it is empty" should {
			"yield empty stream" in {
				Stream.empty.take(1) should be (Stream.empty)
			}
		}

		"it has less elements than required" should {
			"yield all elements" in {
				Stream(1, 2, 3).take(5).toList should be (List(1, 2, 3))
			}
		}

		"it has more elements than required" should {
			"yield first n elements" in {
				Stream(1, 2, 3, 4, 5).take(3).toList should be (List(1, 2, 3))
			}
		}
	}

	"Dropping n elements from Stream" when {
		"it is empty" should {
			"yield that same stream" in {
				val emptyStream = Stream.empty

				emptyStream.drop(5) should be theSameInstanceAs emptyStream
			}
		}

		"it has less elements than required" should {
			"yield empty stream" in {
				Stream(1, 2, 3).drop(5) should be (Stream.empty)
			}
		}

		"it has more elements than required" should {
			"yield remainder of the stream in original order" in {
				Stream(1, 2, 3, 4, 5).drop(3).toList should be (List(4, 5))
			}
		}
	}

	"Taking elements from Stream" when {
		"it is empty" when {
			"predicate holds indefinitely" should {
				"yield that same empty stream" in {
					val emptyStream = Stream.empty

					emptyStream.takeWhile(_ => true) should be theSameInstanceAs emptyStream
				}
			}

			"predicate does not hold at all" should {
				"yield that same empty stream" in {
					val emptyStream = Stream.empty

					emptyStream.takeWhile(_ => false) should be theSameInstanceAs emptyStream
				}
			}
		}

		"it is not empty" when {
			"predicate holds indefinitely" should {
				"yield that entire stream" in {
					Stream(1, 2, 3).takeWhile(_ => true).toList should be (List(1, 2, 3))
				}
			}

			"predicate does not hold at all" should {
				"yield empty stream" in {
					Stream(1, 2, 3).takeWhile(_ => false) should be (Stream.empty)
				}
			}
		}
	}
}