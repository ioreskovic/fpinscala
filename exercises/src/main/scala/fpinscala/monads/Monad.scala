package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par2._
import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, macc) => map2(ma, macc)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List[B]()))((a, mbacc) => map2(f(a), mbacc)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = {
    @scala.annotation.tailrec
    def loop(i: Int, acc: M[List[A]]): M[List[A]] = {
      if (i >= n) acc
      else loop(i + 1, map2(ma, acc)(_ :: _))
    }

    loop(0, unit(List[A]()))

    // we could also do it with sequence, but that depends on foldRight implementation
    // sequence(List.fill(n)(ma))
  }

  def product[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] = {
    @scala.annotation.tailrec
    def loop(ax: List[A], macc: M[List[A]]): M[List[A]] = ax match {
      case a :: as =>
        loop(as, map2(f(a), macc) { case (p, acc) => if (p) a :: acc else acc })
      case Nil => map(macc)(_.reverse)
    }

    loop(ms, unit(List[A]()))
  }

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

  // Implement in terms of `compose`:
  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(f))
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] =
      Par2.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par2.flatMap(ma)(f)
  }

  def parserMonad[P[+ _]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] =
      p.succeed(a)

    override def flatMap[A, B](pa: P[A])(f: A => P[B]): P[B] =
      p.flatMap(pa)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] =
      Some(a)

    override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] =
      oa.flatMap(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] =
      Stream(a)

    override def flatMap[A, B](sa: Stream[A])(f: A => Stream[B]): Stream[B] =
      sa.flatMap(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] =
      List(a)

    override def flatMap[A, B](la: List[A])(f: A => List[B]): List[B] =
      la.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    override def unit[A](a: => A): State[S, A] =
      State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(
        f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A]                        = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B]         = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    override def unit[A](a: => A): Reader[R, A] =
      Reader(_ => a)

    override def flatMap[A, B](st: Reader[R, A])(
        f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }
}
