import org.scalatest._
import org.scalatest.prop._
import org.scalacheck.Gen
import fpinscala.parallelism.Nonblocking._

class NonblockingSpec extends AsyncWordSpec with Matchers with GeneratorDrivenPropertyChecks {
}