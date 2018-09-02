import org.scalatest.WordSpec
import fpinscala.datastructures.List
import fpinscala.datastructures.List.tail

class ListSpec extends WordSpec {

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
	}
	
}
