import org.scalatest.WordSpec
import fpinscala.datastructures.List
import fpinscala.datastructures.List.tail

class ListSpec extends WordSpec {

	"A List" when {
		"it is empty" should {
			"produce NoSuchElementException when tail is invoked" in {
				assertThrows[NoSuchElementException] {
					tail(List())
				}
			}
		}

		"it is singleton" should {
			"produce empty list when tail is invoked" in {
				assert(tail(List(1)) == List())
			}
		}

		"it has more than one element" should {
			"produce another list with all elements in order without head" in {
				assert(tail(List(0, 1, 2, 3)) == List(1, 2, 3))
			}
		}
	}
	
}
