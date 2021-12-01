package common

case class Color(val codes: String*):
  def start = s"\u001b[${codes mkString ";"}m"
  def end = "\u001b[0m"
  def apply(value: Any): String = s"$start$value$end"
  def &(color: Color) = Color(codes ++ color.codes: _*)

object Color:
  val black = Color("30")
  val red = Color("31")
  val green = Color("32")
  val yellow = Color("33")
  val blue = Color("34")
  val bright = Color("1")
  val gray = bright & black
