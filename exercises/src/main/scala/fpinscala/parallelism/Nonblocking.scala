package fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference
import language.implicitConversions

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit,
                                   eh: Throwable => Unit = e =>
                                     throw new RuntimeException(e)): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref   = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1)                              // A latch which, when decremented, implies that `ref` has the result
      p(es)(a => { ref.set(a); latch.countDown }, e => {
        latch.countDown; throw e
      })
      latch.await // Block until the `latch.countDown` is invoked asynchronously
      if (ref.get == null) {
        throw new RuntimeException()
      }
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit, eh: Throwable => Unit = throw (_)): Unit =
            try {
              cb(a)
            } catch {
              case ex: Throwable => eh(ex)
            }
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit, eh: Throwable => Unit = throw (_)): Unit =
            try {
              cb(a)
            } catch {
              case ex: Throwable => eh(ex)
            }
      }

    // How do I add error handler here?
    def fork[A](a: => Par[A]): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit, eh: Throwable => Unit = throw (_)): Unit =
            eval(es)(a(es)(cb, eh))
      }

    /**
      * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
      * This will come in handy in Chapter 13.
      */
    // How do I add error handler here?
    def async[A](f: (A => Unit) => Unit): Par[A] =
      es =>
        new Future[A] {
          def apply(k: A => Unit, eh: Throwable => Unit = throw (_)) =
            try {
              f(k)
            } catch {
              case ex: Throwable => eh(ex)
            }
      }

    /**
      * Helper function, for evaluating an action
      * asynchronously, using the given `ExecutorService`.
      */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C] {
          def apply(cb: C => Unit, eh: Throwable => Unit = throw (_)): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None
            val combiner = Actor[Either[A, B]](es) {
              case Left(a) =>
                if (br.isDefined) eval(es)(cb(f(a, br.get)))
                else ar = Some(a)
              case Right(b) =>
                if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
                else br = Some(b)
            }
            p(es)(a => combiner ! Left(a), eh)
            p2(es)(b => combiner ! Right(b), eh)
          }
      }

    // specialized version of `map`
    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es =>
        new Future[B] {
          def apply(cb: B => Unit, eh: Throwable => Unit = throw (_)): Unit =
            p(es)(a => eval(es) { cb(f(a)) }, eh)
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil    => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    // exercise answers

    /*
     * We can implement `choice` as a new primitive.
     *
     * `p(es)(result => ...)` for some `ExecutorService`, `es`, and
     * some `Par`, `p`, is the idiom for running `p`, and registering
     * a callback to be invoked when its result is available. The
     * result will be bound to `result` in the function passed to
     * `p(es)`.
     *
     * If you find this code difficult to follow, you may want to
     * write down the type of each subexpression and follow the types
     * through the implementation. What is the type of `p(es)`? What
     * about `t(es)`? What about `t(es)(cb)`?
     */
    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit, eh: Throwable => Unit = throw (_)): Unit =
            p(es)(b => {
              if (b) eval(es) { t(es)(cb, eh) } else eval(es) { f(es)(cb, eh) }
            }, eh)
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: A => Unit,
                    eh: Throwable => Unit = e => throw new RuntimeException(e))
            : Unit =
            p(es)(i => {
              eval(es)(ps(i)(es)(cb, eh))
            }, eh)
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A],
                                             ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      ???

    // see `Nonblocking.scala` answers file. This function is usually called something else!
    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      ???

    def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      ???

    def join[A](p: Par[Par[A]]): Par[A] =
      ???

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      ???

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      ???

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    // infix versions of `map`, `map2`
    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B]                     = Par.map(p)(f)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def zip[B](b: Par[B]): Par[(A, B)]                = p.map2(b)((_, _))
    }
  }
}
