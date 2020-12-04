package aoc2020

import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

case object Day4 extends Day:
  type Passport = Map[String, String]
  
  private val Pair = "([^:]+):(\\S+)".r
  
  def parse(lines: Iterator[String], acc: List[Passport] = Nil): Seq[Passport] =
    if lines.isEmpty then acc
    else
      val passport = lines
        .takeWhile(_.nonEmpty)
        .flatMap(_ split " ")
        .map {
          case Pair(key, value) => (key, value)
        }
        .toMap
      parse(lines, passport :: acc)

  type Parser[-A, +B] = A => Try[B]

  def validate[T](f: T => Unit): Parser[T, T] = t => Try { f(t); t }
  
  extension[A,B,C](self: Parser[A, B]):
    def map(f: B => C): Parser[A, C] = x => self(x).map(f)
    def ~> (p: Parser[B, C]): Parser[A, C] = x => self(x).flatMap(p)
    def & (p: Parser[A, C]): Parser[A, (B, C)] = x => for (a <- self(x); b <- p(x)) yield (a, b)
    def | (p: Parser[A, C]): Parser[A, Either[B, C]] = x => self(x) match {
      case Failure(e1) => p(x).map(Right(_)).recoverWith {
        case NonFatal(e2) => 
          val e = new Exception(s"${e1.getMessage}; ${e2.getMessage}")
          e.addSuppressed(e1)
          e.addSuppressed(e2)
          Failure(e)
      }
      case Success(x) => Success(Left(x))
    }
  
  def RequiredField(name: String): Parser[Passport, String] = 
    _.get(name).fold(Failure(new Exception(s"Field '$name' is required'")))(Success(_))
  
  val Integer: Parser[String, Int] = x => Try(x.toInt)
  
  def Between(min: Int, max: Int): Parser[Int, Int] = validate { x =>
    if (x < min || x > max) sys.error(s"$x is not in [$min,$max]")
  }
  
  def Suffix(s: String): Parser[String, String] = {
    case x if x endsWith s => Success(x.stripSuffix(s))
    case x => Failure(new Exception(s"'$x' doesn't have suffix $s"))
  } 
  
  val Cm = Suffix("cm") ~> Integer ~> Between(150, 193)
  val In = Suffix("in") ~> Integer ~> Between(59, 76)
  
  
  def Match(R: Regex): Parser[String, String] = {
    case v @ R() => Success(v)
    case other => Failure(new Exception(s"'$other' doesn't match $R"))
  }
  
  def OneOf[T](ts: T*): Parser[T, T] = {
    val valid = ts.toSet
    validate { x =>
      if (!valid(x)) sys.error(s"$x is not one of ${ts mkString ", "}")
    }
  }
  
  val ValidPassport: Parser[Passport, Any] = 
    RequiredField("byr") &
    RequiredField("iyr") &
    RequiredField("eyr") &
    RequiredField("hgt") &
    RequiredField("hcl") &
    RequiredField("ecl") &
    RequiredField("pid") 
  
  val ValidPassport2: Parser[Passport, Any] =
    (RequiredField("byr") ~> Integer ~> Between(1920, 2002)) &
    (RequiredField("iyr") ~> Integer ~> Between(2010, 2020)) &
    (RequiredField("eyr") ~> Integer ~> Between(2020, 2030)) &
    (RequiredField("hgt") ~> (Cm | In)) &
    (RequiredField("hcl") ~> Match("#[a-f0-9]{6}".r)) &
    (RequiredField("ecl") ~> OneOf("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) &
    (RequiredField("pid") ~> Match("[0-9]{9}".r)) 
  
  override def test(): Unit =
    val sample = parse(
      """
        |ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
        |byr:1937 iyr:2017 cid:147 hgt:183cm
        |
        |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
        |hcl:#cfa07d byr:1929
        |
        |hcl:#ae17e1 iyr:2013
        |eyr:2024
        |ecl:brn pid:760753108 byr:1931
        |hgt:279cm
        |
        |hcl:#cfa07d eyr:2025 pid:166559648
        |iyr:2011 ecl:brn hgt:59in
        |""".stripMargin.linesIterator)
  
    sample.count(p => ValidPassport(p).isSuccess) shouldBe 2
    sample.count(p => ValidPassport2(p).isSuccess) shouldBe 1

  override def star1(): Unit =
    val ps = readInput(parse(_))
    println(s"Valid: ${ps.count(p => ValidPassport(p).isSuccess)}")

  override def star2(): Unit =
    val ps = readInput(parse(_))
    println(s"Valid: ${ps.count(p => ValidPassport2(p).isSuccess)}")
