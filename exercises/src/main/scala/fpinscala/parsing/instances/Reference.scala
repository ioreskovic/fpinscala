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

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object Reference extends Parsers[Parser] {
  implicit def string(s: String): Parser[String] = { state =>
    {
      val i = firstNonmatchingIndex(state.loc.input, s, state.loc.offset)
      if (i == -1) Success(s, s.length)
      else Failure(state.loc.advanceBy(i).toError(s"'$s'"), i != 0)
    }
  }

  implicit def regex(r: Regex): Parser[String] = { state =>
    {
      r.findPrefixOf(state.input) match {
        case None    => Failure(state.loc.toError(s"regex $r"), false)
        case Some(m) => Success(m, m.length)
      }
    }
  }

  def succeed[A](a: A): Parser[A] = { state =>
    Success(a, 0)
  }

  def slice[A](p: Parser[A]): Parser[String] = { state =>
    p(state) match {
      case Success(_, n)     => Success(state.slice(n), n)
      case f @ Failure(_, _) => f
    }
  }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = { state =>
    p(state).mapError(_.push(state.loc, msg))
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A] = { state =>
    p(state).mapError(_.label(msg))
  }

  def attempt[A](p: Parser[A]): Parser[A]                        = ???
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]  = ???
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]          = ???
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???
}
