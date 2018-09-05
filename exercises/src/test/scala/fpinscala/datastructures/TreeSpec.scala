import org.scalatest._
import org.scalatest.prop._
import fpinscala.datastructures.Branch
import fpinscala.datastructures.Leaf
import fpinscala.datastructures.Tree
import fpinscala.datastructures.Tree._

class TreeSpec extends WordSpec with Matchers with GeneratorDrivenPropertyChecks {

	"Calculating size" when {
		"tree is just root" should {
			"yield one" in {
				Tree.size(Leaf(0)) should be (1)
			}
		}

		"tree has a bit more complex structure" should {
			"yield the number of nodes" in {
				Tree.size(Branch(
					Branch(Leaf(1), Leaf(1)),
					Branch(Leaf(1), Leaf(1))
				)) should be (7)
			}
		}
	}

	"Calculating maximum element in Int Tree" when {
		"tree has only one node" should {
			"yield that node's value" in {
				forAll ("root value") { (x: Int) =>
					Tree.maximum(Leaf(x)) should be (x)
				}
			}
		}
	}
}