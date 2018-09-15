package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  def isFalsified = true
}

object Prop {
  type SuccessCount = Int
  type FailedCase   = String
  type TestCases    = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Passed => p.run(n, rng)
      case f      => f
    }
  }

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    run(n, rng) match {
      case Falsified(e, _) => p.tag(e).run(n, rng)
      case p               => p
    }
  }

  def tag(msg: String) = Prop { (n, rng) =>
    run(n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt)
        .map(a => a / Int.MaxValue.toDouble)
        .map(b => b / (1 / (stopExclusive - start)))
        .map(c => c + start)
        .map(d => d.toInt))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State[RNG, Int](rng => rng.nextInt).map(_ < 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(
      State(RNG.double)
        .flatMap(
          w =>
            if (w < g1._2.abs / (g1._2.abs + g2._2.abs)) g1._1.sample
            else g2._1.sample)
    )

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { n =>
    listOfN(n, g)
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] =
    SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))
}
