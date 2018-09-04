import org.scalatest._
import fpinscala.datastructures.List
import fpinscala.datastructures.List._

class ListSpec extends WordSpec with Matchers {

	"A List" when {
		"invoking tail" should {
			"produce NoSuchElementException when it is empty" in {
				assertThrows[NoSuchElementException] {
					tail(List())
				}
			}

			"produce empty list when it has only one element" in {
				assert(tail(List(1)) == List())
			}

			"produce another list with all elements in order without head" in {
				assert(tail(List(0, 1, 2, 3)) == List(1, 2, 3))
			}
		}

		"setting head" should {
			"produce NoSuchElementException list when invoked on empty list" in {
				assertThrows[NoSuchElementException] {
					setHead(List(), 1)
				}
			}

			"produce singleton list when invoked on singleton list" in {
				assert(setHead(List(0), 1) == List(1))
			}

			"produce tail of original list with prepended element" in {
				assert(setHead(List(0, 2, 3, 4), 1) == List(1, 2, 3, 4))
			}
		}

		"dropping zero elements" should {
			"produce same list" in {
				val list = List(1, 2, 3, 4)

				drop(list, 0) should be theSameInstanceAs list
			}
		}

		"dropping all elements" should {
			"produce empty list" in {
				assert(drop(List(1, 2, 3, 4), 4) == List())
			}
		}

		"dropping less than all elements" should {
			"produce non empty list" in {
				assert(drop(List(1, 2, 3, 4), 2) == List(3, 4))
			}
		}

		"dropping more than all elements" should {
			"produce NoSuchElementException" in {
				assertThrows[NoSuchElementException] {
					drop(List(1, 2, 3, 4), 10)
				}
			}
		}

		"dropping elements while predicate holds" when {
			"list is empty" should {
				"produce empty list" in {
					assert(dropWhile(List(), (_: Any) => true) == List())
				}
			}

			"list has one element" should {
				"produce empty list" in {
					assert(dropWhile(List(1), (_: Int) => true) == List())
				}
			}

			"list has more elements" should {
				"produce list without matching prefix list" in {
					assert(dropWhile(List(-1, -2, 2, 1, -5), (i: Int) => i < 0) == List(2, 1, -5))
				}
			}
		}

		"dropping elements when predicate does not hold" when {
			"should produce equal list" in {
				assert(dropWhile(List(1, 2, 3), (i: Int) => i < 0) == List(1, 2, 3))
			}
		}

		"reversing" when {
			"empty" should {
				"produce empty list" in {
					assert(reverse(List()) == List())
				}
			}

			"singleton" should {
				"produce equal singleton list" in {
					assert(reverse(List(3)) == List(3))
				}
			}

			"has more elements" should {
				"produce list with elements in reverse order" in {
					assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
				}
			}

			"already reversed list" should {
				"produce original list" in {
					assert(reverse(reverse(List(1, 2, 3))) == List(1, 2, 3))
				}
			}
		}

		"calling init" when {
			"it is empty" should {
				"produce empty list" in {
					assert(init(List()) == List())
				}
			}

			"it is singleton" should {
				"produce empty list" in {
					assert(init(List(1)) == List())
				}
			}

			"it has more elements" should {
				"produce all but last" in {
					assert(init(List(1, 2, 3)) == List(1, 2))
				}
			}
		}

		"computing length" when {
			"it is empty" should {
				"produce zero" in {
					assert(List.length(List()) == 0)
				}
			}

			"it is singleton list" should {
				"produce one" in {
					assert(List.length(List(33)) == 1)
				}
			}

			"it has more elements" should {
				"produce element count" in {
					assert(List.length(List(1, 4, 7)) == 3)
				}
			}
		}

		"filtering elements" when {
			"no elements match" should {
				"produce original list" in {
					assert(filter(List(2, 4, 6, 8))(_ % 2 == 0) == List(2, 4, 6, 8))
				}
			}

			"all elements match" should {
				"produce empty list" in {
					assert(filter(List(1, 3, 5, 7))(_ % 2 == 0) == List())
				}
			}

			"some elements match" should {
				"produce list containig elements that satisfy predicate" in {
					assert(filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
				}
			}
		}

		"flatmapping elements" should {
			"apply function and flatten result" in {
				assert(flatMap(List(0, 1))(x => List(x * 2, x * 2 + 1)) == List(0, 1, 2, 3))
			}
		}
	}

	"An empty list" when {
		"appended by empty list" should {
			"produce empty list" in {
				assert(append(List(), List()) == List())
			}
		}

		"appended by non-empty list" should {
			"produce that non empty list" in {
				assert(append(List(), List(1, 2, 3)) == List(1, 2, 3))
			}
		}
	}

	"A non empty list" when {
		"appended by empty list" should {
			"produce original list" in {
				assert(append(List(1, 2, 3), List()) == List(1, 2, 3))
			}
		}

		"appended by non-empty list" should {
			"produce list containing all elements in order a1:a2" in {
				assert(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
			}
		}
	}

	"An Int list" when {
		"computing sum" when {
			"it is empty" should {
				"produce default value (zero)" in {
					assert(sum(List()) == 0)
				}
			}

			"it is singleton list" should {
				"produce value of element" in {
					assert(sum(List(33)) == 33)
				}
			}

			"it has more elements" should {
				"produce sum value of all elements" in {
					assert(sum(List(-1, 2, -3, 4, -5, 6, 0)) == 3)
				}
			}
		}

		"adding one to each element" when {
			"it is empty" should {
				"produce empty list" in {
					assert(addOne(List()) == List())
				}
			}

			"it is not empty" should {
				"produce non empty list in original order" in {
					assert(addOne(List(1, 2, 3)) == List(2, 3, 4))
				}
			}
		}
	}

	"An Double list" when {
		"computing product" when {
			"it is empty" should {
				"produce default value (one)" in {
					assert(product(List()) == 1.0)
				}
			}

			"it is singleton list" should {
				"produce value of element" in {
					assert(product(List(-1.0)) == -1.0)
				}
			}

			"it has more elements" should {
				"produce product value of all elements" in {
					assert(product(List(-1, 2, -3, 4, -5, 6, 0)) == 0)
				}
			}
		}

		"stringifying elements" when {
			"it is empty" should {
				"produce empty list" in {
					assert(doubleString(List()) == List())
				}
			}

			"it is not empty" should {
				"produce list with string elements" in {
					assert(doubleString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
				}
			}
		}
	}

	"A List of lists" when {
		"computing flattened list" when {
			"it is empty" should {
				"produce empty list" in {
					assert(flatten(List()) == List())
				}
			}

			"it is singleton list" should {
				"produce that list" in {
					assert(flatten(List(List(1, 2, 3))) == List(1, 2, 3))
				}
			}

			"it consists of many lists" should {
				"produce list with elements in prodvided order" in {
					assert(flatten(List(
						List(), 
						List(1), 
						List(2, 2), 
						List(3, 3, 3)
					)) == List(1, 2, 2, 3, 3, 3))
				}
			} 
		}
	}

	"A pair of lists" when {
		"zipping them together" when {
			"both are empty" should {
				"produce empty list" in {
					assert(zipWith(List[Int](), List[Double]())
						((i, d) => (i + d).toString) == List())
				}
			}

			"both are non-empty and of same length" should {
				"produce zipped list" in {
					assert(zipWith(List(1, 2, 3), List(1.0, 2.0, 3.0))
						((i, d) => (i + d).toString) == List("2.0", "4.0", "6.0"))
				}
			}

			"they are of different lengths" should {
				"zip on aligning elements and ignore rest" in {
					assert(zipWith(List(1, 2), List(1.0, 2.0, 3.0, 4.0))
						((i, d) => (i + d).toString) == List("2.0", "4.0"))
				}
			}
		}
	}
	
}
