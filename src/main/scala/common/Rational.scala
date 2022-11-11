package common

case class Rational private (nominator: Long, denominator: Long):
  assume(denominator > 0)

  override def toString: String =
    if denominator == 1 then nominator.toString
    else s"$nominator/$denominator"

  def * (factor: Long): Rational = (factor * nominator) %% denominator
  def / (factor: Long): Rational = nominator %% (factor * denominator)

extension (x: Long)
  def toRational = Rational(x)
  def %% (y: Long) = Rational(x, y)
  def * (r: Rational) = r * x
  def / (r: Rational) = r.denominator * x %% r.nominator

extension (s: String) def toRational: Rational =
  Rational.parseString(s).getOrElse(throw NumberFormatException(s"Invalid rational $s"))

object Rational extends Fractional[Rational]:

  def apply(i: Long): Rational = Rational(i, 1)

  def apply(n: Long, d: Long): Rational =
    if d == 0 then throw new IllegalArgumentException("Denominator can't be 0")
    if n == 0 then new Rational(0, 1)
    else
      val k = math.signum(d) * gcd(n, d)
      new Rational(n / k, d / k)

  // Numeric

  override def plus(x: Rational, y: Rational): Rational =
    val k = lcm(x.denominator, y.denominator)
    (x.nominator * (k / x.denominator) + y.nominator * (k / y.denominator)) %% k

  override def minus(x: Rational, y: Rational): Rational =
    val k = lcm(x.denominator, y.denominator)
    (x.nominator * (k / x.denominator) - y.nominator * (k / y.denominator)) %% k

  override def times(x: Rational, y: Rational): Rational =
    Rational(x.nominator * y.nominator, x.denominator * y.denominator)

  override def negate(x: Rational): Rational = new Rational(-x.nominator, x.denominator)

  override def fromInt(x: Int): Rational = Rational(x)

  override def parseString(str: String): Option[Rational] = str.split("/") match
    case Array(n, d) =>
      for n <- Numeric.LongIsIntegral.parseString(n)
          d <- Numeric.LongIsIntegral.parseString(d)
        yield n %% d
    case Array(x) => Numeric.LongIsIntegral.parseString(x).map(Rational(_))
    case _ => None

  override def toLong(x: Rational): Long = x.nominator / x.denominator

  override def toInt(x: Rational): Int = toLong(x).toInt

  override def toDouble(x: Rational): Double = x.nominator.toDouble / x.denominator

  override def toFloat(x: Rational): Float = toDouble(x).toFloat

  override def abs(x: Rational): Rational = new Rational(math.abs(x.nominator), x.denominator)

  // Ordering

  override def compare(x: Rational, y: Rational): Int =
    if x == y then 0
    else
      val k = lcm(x.denominator, y.denominator)
      val xn = x.nominator * (k / x.denominator)
      val yn = y.nominator * (k / y.denominator)
      if xn < yn then -1
      else 1

  // Fractional

  override def div(x: Rational, y: Rational): Rational =
    (x.nominator * y.denominator) %% (y.nominator * x.denominator)
