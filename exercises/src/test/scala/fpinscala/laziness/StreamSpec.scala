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

	"Checking if all elements of a stream satisfy predicate" when {
		"stream is empty" should {
			"yield true" in {
				Stream().forAll(_ => true) should be (true)
			}
		}

		"stream is not empty" when {
			"not all elements hold" should {
				"yield false" in {
					Stream(1, 2, 3).forAll(_ < 2) should be (false)
				}
			}

			"all elements hold" should {
				"yield true" in {
					Stream(1, 2, 3).forAll(_ => true) should be (true)
				}
			}
		}
	}

	"Extracting head" when {
		"stream is empty" should {
			"yield None" in {
				Stream.empty[Int].headOption should be (None)
			}
		}

		"stream is not empty" should {
			"yield some first element" in {
				Stream(1, 2, 3).headOption should be (Some(1))
			}
		}
	}

	"Mapping over stream" should {
		"yield transformed stream" in {
			val f: Int => String = _.toString

			Stream(1, 2, 3).map(f).toList should be (List(f(1), f(2), f(3)))
		}
	}

	"Filtering over stream" should {
		"retain matching elements in order" in {
			Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList should be (List(2, 4))
		}
	}

	"Appending to stream" when {
		"original is empty" should {
			"yield other stream" in {
				val secondStream = Stream(1, 2, 3)

				Stream.empty[Int].append(secondStream).toList should be (secondStream.toList)
			}
		}

		"original is not empty" should {
			"yield stream concatenation" in {
				val firstStream = Stream(1, 2, 3)
				val secondStream = Stream(4, 5, 6)

				firstStream.append(secondStream).toList should be (firstStream.toList ::: secondStream.toList)
			}
		}

		"second stream is empty" should {
			"yield first stream" in {
				val firstStream = Stream(1, 2, 3)

				firstStream.append(Stream.empty).toList should be (firstStream.toList)
			}
		}
	}

	"Flatmapping over stream" should {
		"yield all elements in order" in {
			Stream(1, 10).flatMap(i => Stream(i, i + 1, i + 2).map(_.toString))
				.toList should be (List("1", "2", "3", "10", "11", "12"))
		}
	}

	"Creating constant stream" should {
		"always contain same constant element" in {
			val element = 3
			val n = 5
			Stream.constant(element).take(n).toList should be (List.fill(n)(element))
		}
	}

	"Creating increasing int stream" should {
		"yield elements in increasing order" in {
			Stream.from(0).take(5).toList should be (List(0, 1, 2, 3, 4))
		}
	}

	"Creating Fibonnaci stream" should {
		"yield Fibonnaci sequence elements" in {
			Stream.fibs.take(5).toList should be (List(0, 1, 1, 2, 3))
		}
	}

	"Zipping two streams" should {
		"produce a stream with size of shorter stream" in {
			val s1 = Stream(1, 2, 3)
			val s2 = Stream(1.0, 2.0)
			val f: (Int, Double) => String = (i, d) => (i + d).toString

			s1.zipWith(s2)(f).toList should be (List("2.0", "4.0"))
		}
	}

	"Zipping all of two streams" should {
		"produce elements as long any stream has more elements" in {
			val s1 = Stream(1, 2, 3)
			val s2 = Stream('a')

			s1.zipAll(s2).toList should be (List(
				(Some(1), Some('a')),
				(Some(2), None),
				(Some(3), None)
			))
		}
	}
}
