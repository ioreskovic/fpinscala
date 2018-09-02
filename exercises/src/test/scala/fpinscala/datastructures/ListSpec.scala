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
			"produce empty list" in {
				assert(drop(List(1, 2, 3, 4), 10) == List())
			}
		}
	}
	
}
