package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 1
    case Branch(left, right) => size(left) + 1 + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x)             => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)             => 0
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a)             => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(c: (B, B) => B): B = t match {
    case Leaf(x)      => f(x)
    case Branch(l, r) => c(fold(l)(f)(c), fold(r)(f)(c))
  }
}
