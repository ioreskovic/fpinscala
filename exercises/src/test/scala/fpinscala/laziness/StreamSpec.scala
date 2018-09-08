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
}