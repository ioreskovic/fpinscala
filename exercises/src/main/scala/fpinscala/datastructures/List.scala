package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, acc) => Cons(x, acc))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => throw new NoSuchElementException
    case Cons(x, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil         => throw new NoSuchElementException
        case Cons(_, ls) => drop(ls, n - 1)
      }
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil                 => Nil
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def init[A](l: List[A]): List[A] = reverse(l) match {
    case Cons(_, xs) => reverse(xs)
    case Nil         => Nil
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def flatten[A](lists: List[List[A]]): List[A] =
    foldRight(lists, Nil: List[A])((list, acc) => append(list, acc))

  def addOne(intList: List[Int]): List[Int] = map(intList)(_ + 1)

  def doubleString(doubleList: List[Double]): List[String] =
    map(doubleList)(_.toString)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def zipWith[A, B, C](aList: List[A], bList: List[B])(
      f: (A, B) => C): List[C] = {

    @annotation.tailrec
    def loop(ax: List[A], bx: List[B], cx: List[C] = Nil): List[C] =
      (ax, bx) match {
        case (Cons(a, as), Cons(b, bs)) => loop(as, bs, Cons(f(a, b), cx))
        case _                          => cx
      }

    reverse(loop(aList, bList))
  }

  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(a, as), Cons(b, bs)) if (a == b) => startsWith(as, bs)
    case (_, Nil)                               => true
    case _                                      => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, ss)               => hasSubsequence(ss, sub)
  }

}
