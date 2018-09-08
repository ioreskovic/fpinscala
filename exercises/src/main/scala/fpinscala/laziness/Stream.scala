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

  def take(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1)            => Some((h(), (empty, 0)))
    case (Cons(h, t), i) if (i > 1) => Some((h(), (t(), i - 1)))
    case _                          => None
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _                     => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _                    => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, x) => p(a) && x)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def toList: List[A] = foldLeft(Nil: List[A])((acc, a) => a :: acc).reverse

  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _          => None
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, bs) => if (p(a)) cons(a, bs) else bs)

  def append[B >: A](sb: => Stream[B]): Stream[B] =
    foldRight(sb)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, bs) => f(a).append(bs))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, that) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, that) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _                     => None
    }

  def startsWith[B](that: Stream[B]): Boolean =
    zipAll(that).takeWhile(!_._2.isEmpty).forAll {
      case (maybeA, maybeB) => maybeA == maybeB
    }

  def tails: Stream[Stream[A]] = scanRight(empty[A])(cons(_, _))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, state) => {
      lazy val (currHead, acc) = state
      val newElem = f(a, currHead)
      (newElem, cons(newElem, acc))
    })._2
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

  val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  val fibs: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def from(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None         => empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
}
