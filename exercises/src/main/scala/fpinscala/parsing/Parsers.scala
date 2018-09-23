package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions

trait Parsers[Parser[+ _]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // primitive?
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  // primitive
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(f andThen succeed)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _).or(succeed(Nil))

  // primitive
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  // primitive
  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    map2(p, p2)((_, _))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p))(_ :: _)

  // primitive
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // primitive?
  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] =
      self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(a => run(succeed(p))(a) == Right(a))
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {}
