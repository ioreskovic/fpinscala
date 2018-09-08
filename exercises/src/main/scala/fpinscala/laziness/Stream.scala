package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  @annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
    case Cons(h, t) => t().foldLeft(f(z, h()))(f)
    case _          => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if (n == 1) => cons(h(), empty)
    case Cons(h, t) if (n > 1)  => cons(h(), t().take(n - 1))
    case _                      => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, s) => if (p(a)) cons(a, s) else s)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, x) => p(a) && x)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] = foldLeft(Nil: List[A])((acc, a) => a :: acc).reverse

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, bs) => cons(f(a), bs))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, bs) => if (p(a)) cons(a, bs) else bs)

  def append[B >: A](sb: => Stream[B]): Stream[B] =
    foldRight(sb)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a).append(bs))
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty                                   extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  val fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = Stream.cons(a, loop(b, b + a))

    loop(0, 1)
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
}
