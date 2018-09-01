import org.scalacheck.Gen.choose
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import fpinscala.gettingstarted.MyModule.fib

object FibonnaciSpecification extends Properties("FibonnaciNumber") {
	val fibFirst = choose(1, 1)
	val fibSecond = choose(2, 2)
	val fibOthers = choose(3, 10)

	property("first") = forAll (1) { nFib: Int => 
		fib(nFib) == 0
	}

	property("second") = forAll (2) { nFib: Int =>
		fib(nFib) == 1
	}

	property("others") = forAll (fibOthers) { nFib: Int =>
		fib(nFib) == fib(nFib - 2) + fib(nFib - 1)
	}
}