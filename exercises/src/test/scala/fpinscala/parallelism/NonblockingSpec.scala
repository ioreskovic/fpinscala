import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.parallelism.Nonblocking._
import java.util.concurrent.{ Callable, ExecutorService, Executors }


class NonblockingSpec extends AsyncWordSpec with Matchers with GeneratorDrivenPropertyChecks {
	val es: ExecutorService = Executors.newWorkStealingPool(8)

	"computing unit" should {
		"yield value" in {
			val parInt = Par.unit(42)
			Par.run(es)(parInt) should be (42)
		}
	}

	"computing deferred unit" should {
		"eventually complete with exception" in {
			val delayedInt = Par.delay[Int]({
				throw new IllegalArgumentException()
				42
			})

			assertThrows[RuntimeException] {
				Par.run(es)(delayedInt) should be (42)
			}
		}
	}

	"forking" should {
		"eventually complete with exception" in {
			val forkedInt = Par.fork[Int]({
				Par.delay[Int]({
					throw new IllegalArgumentException()
					42
				})
			})

			assertThrows[RuntimeException] {
				Par.run(es)(forkedInt) should be (42)
			}
		}
	}

	"nested forking" should {
		"eventually complete with exception" in {
			val forkedInt = Par.fork[Int]({
				Par.fork[Int]({
					Par.fork[Int]({
						Par.delay[Int]({
							throw new IllegalArgumentException()
							42
						})
					})
				})
			})

			assertThrows[RuntimeException] {
				Par.run(es)(forkedInt) should be (42)
			}
		}
	}
}