import Gen._
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (f andThen succeed)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a,b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if(n > 0)
      map2(p, listOfN(n - 1, p))(_ :: _)
    else
      succeed(List())

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a,b)

  def manyTwo[A](p1: Parser[A], p2: Parser[A]): Parser[(Int, Int)]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a )

  def fail[A](p: Parser[A]): ParseError

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset+1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)
    (implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: Parser[B]) = self.product(p, p2)

    def **[B](p2: Parser[B]) = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}
