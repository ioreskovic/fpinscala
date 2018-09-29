package fpinscala
package parsing

import ReferenceTypes._
import scala.util.matching.Regex

object ReferenceTypes {
  type Parser[+A] = ParseState => Result[A]

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }

  sealed trait Result[+A] {
    def extract: Either[ParseError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(a, _) => Right(a)
    }
    /* Used by `attempt`. */
    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }
    /* Used by `flatMap` */
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }
    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _             => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _             => this
    }
  }
  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean)
      extends Result[Nothing]
}

object Reference extends Parsers[Parser] {
  def attempt[A](p: Parser[A]): Parser[A]                        = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]  = ???
  def label[A](msg: String)(p: Parser[A]): Parser[A]             = ???
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]          = ???
  implicit def regex(r: Regex): Parser[String]                   = ???
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
  def scope[A](msg: String)(p: Parser[A]): Parser[A]             = ???
  def slice[A](p: Parser[A]): Parser[String]                     = ???
  implicit def string(s: String): Parser[String]                 = ???
  def succeed[A](a: A): Parser[A]                                = ???
}
