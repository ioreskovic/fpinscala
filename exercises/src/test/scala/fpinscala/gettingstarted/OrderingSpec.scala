import org.scalatest.FlatSpec
import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

class OrderingSpec extends FlatSpec {

	val gt: (Int, Int) => Boolean = (x, y) => x > y

	"An empty array" should "be ordered" in {
		assert(isSorted(Array.empty, gt) == true)
	}

	"A singleton array" should "be ordered" in {
		assert(isSorted(Array(-42), gt) == true)
	}

	"An array with same elements" should "be ordered" in {
		assert(isSorted(
			Array.fill(10)(42),
			gt
		) == true)
	}

	"A sorted array" should "be ordered" in {
		import scala.util.Random

		val random = new Random()
		val array = Array.fill(10)(random.nextInt)

		assert(isSorted(
			array.sortWith(_ < _),
			gt
		) == true)
	}

	"A non-sorted array" should "not be ordered" in {
		assert(isSorted(
			Array(1, 2, 3, 4, 3),
			gt
		) == false)
	}
}