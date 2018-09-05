package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depth[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((ld, rd) => 1 + (ld max rd))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

  def fold[A, B](t: Tree[A])(f: A => B)(c: (B, B) => B): B = t match {
    case Leaf(x)      => f(x)
    case Branch(l, r) => c(fold(l)(f)(c), fold(r)(f)(c))
  }
}
