package aoc2020

object coord:
  case class Int2(x: Int, y: Int):
    override def toString: String = s"($x; $y)"
    def + (that: Int2) = Int2(x + that.x, y + that.y)
    def - (that: Int2) = Int2(x - that.x, y - that.y)
    def * (f: Int) = Int2(x*f, y*f)
    def norm = math.abs(x) + math.abs(y)
    def rotateTimes(times: Int) =
      val left = if times < 0 then math.ceil(-times / 4.0).toInt * 4 + times else times
      (1 to left).foldLeft(this) {
        case (Int2(x, y), _) => Int2(-y, x)
      }
      // (2, 1) - left => (-1, 2)
  
  object Int2:
    val zero = Int2(0,0)

  enum Dir(val delta: Int2):
    case N extends Dir(Int2(0,1))
    case E extends Dir(Int2(1,0))
    case S extends Dir(Int2(0,-1))
    case W extends Dir(Int2(-1, 0))
    def left = this match
      case N => W
      case E => N
      case S => E
      case W => S
    def right = this match
      case N => E
      case E => S
      case S => W
      case W => N
    def turn(left: Boolean, times: Int): Dir =
      if left then (1 to times).foldLeft(this)((d, _) => d.left)
      else (1 to times).foldLeft(this)((d, _) => d.right)
    def turn(degrees: Int): Dir =
      turn(left = true, (360 + degrees) % 360 / 90)