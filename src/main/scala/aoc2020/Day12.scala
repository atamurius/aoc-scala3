package aoc2020

import aoc2020.coord._

case object Day12 extends Day:
  
  enum Op:
    case N, S, E, W, L, R, F
  
  case class Instr(op: Op, value: Int):
    override def toString: String = s"$op$value"
  
  def parse(line: String) = Instr(Op.valueOf(line.take(1)), line.drop(1).toInt)

  case class Position(angle: Dir = Dir.E, pos: Int2 = Int2.zero):
    override def toString: String = s"$pos, $angle"

    def move(instr: Instr) = instr match
      case Instr(Op.N, value) => copy(pos = pos + Dir.N.delta * value)
      case Instr(Op.S, value) => copy(pos = pos + Dir.S.delta * value)
      case Instr(Op.E, value) => copy(pos = pos + Dir.E.delta * value)
      case Instr(Op.W, value) => copy(pos = pos + Dir.W.delta * value)
      case Instr(Op.L, deg) => copy(angle = angle.turn(deg))
      case Instr(Op.R, deg) => copy(angle = angle.turn(-deg))
      case Instr(Op.F, value) => copy(pos = pos + angle.delta * value)
    
    def moveAll(is: IterableOnce[Instr], trace: Boolean = false) = is.iterator.foldLeft(this) {
      (curr, i) =>
        val next = curr move i
        if trace then println(s"$i -> $next")
        next
    }
    def manhattan = pos.norm
  
  case class Ship(waypoint: Int2 = Int2(10, 1), pos: Int2 = Int2.zero):
    override def toString: String = s"$pos, waypoint $waypoint"
    def move(instr: Instr) = instr match
      case Instr(Op.N, value) => copy(waypoint = waypoint + Dir.N.delta * value)
      case Instr(Op.S, value) => copy(waypoint = waypoint + Dir.S.delta * value)
      case Instr(Op.E, value) => copy(waypoint = waypoint + Dir.E.delta * value)
      case Instr(Op.W, value) => copy(waypoint = waypoint + Dir.W.delta * value)
      case Instr(Op.F, value) => copy(pos = pos + waypoint * value)
      case Instr(Op.L, value) => copy(waypoint = waypoint.rotateTimes(value % 360 / 90))
      case Instr(Op.R, value) => copy(waypoint = waypoint.rotateTimes(value % 360 / -90))
    def moveAll(is: IterableOnce[Instr], trace: Boolean = false) = is.iterator.foldLeft(this) {
      (curr, i) =>
        val next = curr move i
        if trace then println(s"$i -> $next")
        next
    }
  
  override def test(): Unit =
    val sample =
      """
        |F10
        |N3
        |F7
        |R90
        |F11
        |""".stripMargin.trim.linesIterator.map(parse).toVector
    Position().moveAll(sample).manhattan shouldBe 25
    Ship().moveAll(sample).pos.norm shouldBe 286
    val sample2 = 
      """
        |F10
        |L90
        |F10
        |L90
        |F10
        |L90
        |F10
        |""".stripMargin.trim.linesIterator.map(parse)
    Position().moveAll(sample2).manhattan shouldBe 0

  override def star1(): Any =
    readInput { i =>
      Position().moveAll(i map parse).manhattan
    }

  override def star2(): Any = readInput(i => Ship().moveAll(i map parse).pos.norm)
