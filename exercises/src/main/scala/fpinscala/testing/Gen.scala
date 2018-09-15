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

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  def &&(other: Prop): Prop = new Prop {
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase   = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))
}

trait SGen[+A] {}
