package common

case class Color(val codes: String*):
  def start = s"\u001b[${codes mkString ";"}m"
  def end = "\u001b[0m"
  def apply(value: Any): String = s"$start$value$end"
  def &(color: Color) = Color(codes ++ color.codes: _*)
  def and(f: => Color.type => Color): Color = &(f(Color))

object Color:
  val black = Color("30")
  val white = Color("37")
  val red = Color("31")
  val green = Color("32")
  val yellow = Color("33")
  val blue = Color("34")
  val bright = Color("1")
  val gray = bright & black

  val bgBlue = Color("44")
  val negative = Color("7")

  val levels = {
    val chars = ".:-=+*#%@"
    chars.map(Color.blue(_)) ++
      chars.map(Color.green(_)) ++
      chars.map(Color.gray(_)) ++
      chars.map(Color.white(_)) ++
      chars.map(Color.white.and(_.bright)(_))
  }

  def displayLevel(value: Double): String = levels((value * (levels.size - 1)).toInt)
