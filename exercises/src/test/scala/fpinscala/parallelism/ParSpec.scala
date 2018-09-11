import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.parallelism.Par
import fpinscala.parallelism.Par._

import java.util.concurrent.{ Callable, Future, ExecutorService, Executors }

class ParSpec extends AsyncWordSpec with Matchers with GeneratorDrivenPropertyChecks {
	val es: ExecutorService = Executors.newCachedThreadPool()

	"map2" should {
		"eventually combine results of 2 futures" in {
			val parC = Par.map2(Par.unit(1), Par.unit(2.0))((i, d) => (i + d).toString)

			parC(es).get should be ("3.0")
		}
	}

	"async computation" should {
		"eventually yield" in {
			val f: Int => Double = i => i + 1.0
			val a = 2
			Par.asyncF(f)(a)(es).get should be (f(a))
		}
	}
}