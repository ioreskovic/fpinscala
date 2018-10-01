package fpinscala.monoids

import fpinscala.parallelism.Nonblocking2._
import fpinscala.parallelism.Nonblocking2.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

trait Ord[A] {
  def inOrder(x: A, y: A): Boolean
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero                       = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero                         = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero                 = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    def zero                 = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero                         = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero                         = true
  }

  def optionMonoid[A](implicit ev: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def op(a1: Option[A], a2: Option[A]) = a1 match {
        case None => a2
        case Some(x) =>
          a2 match {
            case None    => a1
            case Some(y) => Some(ev.op(x, y))
          }
      }

      def zero = None
    }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(x: A => A, y: A => A) = x andThen y
    def zero                     = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero              = m.zero
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associativity = forAll(for {
      a <- gen
      b <- gen
      c <- gen
    } yield (a, b, c)) {
      case (x, y, z) =>
        m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    }

    val identity =
      forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

    associativity && identity
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length > 1) {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    } else if (as.length == 1) {
      f(as.head)
    } else {
      m.zero
    }
  }

  private def minMonoid: Monoid[Int] = new Monoid[Int] {
    def zero                    = Int.MaxValue
    def op(x: Int, y: Int): Int = x min y
  }

  private def maxMonoid: Monoid[Int] = new Monoid[Int] {
    def zero                    = Int.MinValue
    def op(x: Int, y: Int): Int = x max y
  }

  private def asc: Ord[Int] = new Ord[Int] {
    def inOrder(x: Int, y: Int): Boolean = x <= y
  }

  private def desc: Ord[Int] = new Ord[Int] {
    def inOrder(x: Int, y: Int): Boolean = x >= y
  }

  type IntOrder = (Monoid[Int], Monoid[Int], Ord[Int])

  val AscOrder: IntOrder  = (minMonoid, maxMonoid, asc)
  val DescOrder: IntOrder = (maxMonoid, minMonoid, desc)

  def ordered(ints: IndexedSeq[Int])(o: IntOrder = AscOrder): Boolean = {
    type Ord    = Boolean
    type OrdSeg = Option[(Int, Int, Boolean)]

    val mon = new Monoid[OrdSeg] {
      def zero = None
      def op(x: OrdSeg, y: OrdSeg): OrdSeg = (x, y) match {
        case (Some((xStart, xEnd, xOrd)), Some((yStart, yEnd, yOrd))) => {
          Some(o._1.op(xStart, yStart),
               o._2.op(xEnd, yEnd),
               xOrd && yOrd && o._3.inOrder(xEnd, yStart))
        }
        case (None, yy) => yy
        case (xx, None) => xx
      }
    }

    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String)                            extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero                               = Par.unit(m.zero)
    def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.async(b => ()))
    }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero: WC = Stub("")
    def op(x: WC, y: WC): WC = (x, y) match {
      case (Stub(sx), Stub(sy))         => Stub(sx + sy)
      case (Stub(sx), Part(ly, cy, ry)) => Part(sx + ly, cy, ry)
      case (Part(lx, cx, rx), Stub(sy)) => Part(lx, cx, rx + sy)
      case (Part(lx, cx, rx), Part(ly, cy, ry)) if (rx + ly).isEmpty =>
        Part(lx, cy + cy, ry)
      case (Part(lx, cx, rx), Part(ly, cy, ry)) => Part(lx, cy + cy + 1, ry)
    }
  }

  def count(s: String): Int = {
    def w: String => Int = s => if (s.trim.isEmpty) 0 else 1
    def m: Char => WC    = c => if (c == ' ') Part("", 0, "") else Stub(c.toString)

    foldMapV(s, wcMonoid)(m) match {
      case Stub(s)       => w(s)
      case Part(l, c, r) => w(l) + c + w(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a)      => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None    => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None    => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None    => z
    case Some(a) => f(a, z)
  }
}
