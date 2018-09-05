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

		"tree has multiple elements" should {
			"yield max element value" in {
				Tree.maximum(Branch(
					Branch(Leaf(1), Leaf(3)),
					Branch(Leaf(5), Leaf(7))
				)) should be (7)
			}
		}
	}

	"Calculating depth" when {
		"tree has only root" should {
			"yield zero" in {
				forAll ("root value") { (x: Int) =>
					Tree.depth(Leaf(x)) should be (0)
				}
			}
		}

		"tree has multiple levels" should {
			"yield max depth" in {
				Tree.depth(Branch(
					Leaf(1), Branch(
						Leaf(2), Branch(
							Leaf(3), Branch(
								Leaf(4), Leaf(4)
							)
						)
					)
				)) should be (4)
			}
		}
	}

	"Mapping over tree" should {
		"apply mapping function to all elements" in {
			Tree.map(
				Branch(
					Leaf(1), Branch(
						Leaf(2), Branch(
							Leaf(3), Branch(
								Leaf(4), Leaf(4)
							)
						)
					)
				)
			)(_.toString) should be (
				Branch(
					Leaf("1"), Branch(
						Leaf("2"), Branch(
							Leaf("3"), Branch(
								Leaf("4"), Leaf("4")
							)
						)
					)
				)
			)
		}
	}
}