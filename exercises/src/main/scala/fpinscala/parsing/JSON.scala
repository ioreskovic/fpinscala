package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull                          extends JSON
  case class JNumber(get: Double)            extends JSON
  case class JString(get: String)            extends JSON
  case class JBool(get: Boolean)             extends JSON
  case class JArray(get: IndexedSeq[JSON])   extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def jNum     = double.map(JNumber)
    def jStr     = escapedQuoted.map(JString)
    def jNull    = "null".as(JNull)
    def jTrue    = "true".as(JBool(true))
    def jFalse   = "false".as(JBool(false))
    def jBool    = jTrue | jFalse
    def jLiteral = jNull | jStr | jNum | jBool

    def jVal: Parser[JSON] = jLiteral | jObj | jArr
    def jKv                = escapedQuoted ** (":" *> jVal)

    def jArr =
      surround("[", "]")(jVal.sep(",").map(items => JArray(items.toIndexedSeq)))
    def jObj =
      surround("{", "}")(jKv.sep(",").map(pairs => JObject(pairs.toMap)))

    root(whitespace *> (jObj | jArr))
  }
}
