package aoc2019

import common.*
import IntCode.*
import common.coord.*
import common.read.Board

import scala.annotation.tailrec
import scala.collection.Iterator.from

case object Day19 extends Day:
  def inputCode: Machine = machine(readInput(_.next()))

  override def star1(): Any =
    val code = inputCode
    val board = Board(
      Vector.tabulate(50) { y =>
        Vector.tabulate(50) { x =>
          val Seq(res) = code.runIO(x, y)
          if res > 0 then "#" else "."
        }
      }
    )
    board.points.count(_._2 == "#")

  override def star2(): Any =
    import GridDir.*
    val code = inputCode
    def isBeamed(p: Int2) = code.runIO(p.x, p.y).head == 1
    def nonBeamed(p: Int2) = !isBeamed(p)
    def lookup(p: Int2, d: Dir, cond: Int2 => Boolean): Int2 =
      Iterator.iterate(p)(_ + d.delta).find(cond).get

    val y = 100
    val left = lookup(Int2(0, y), R, isBeamed)
    val right = lookup(left, R, nonBeamed) + L.delta
    val minAngle = left.x.toDouble / left.y
    val maxAngle = right.x.toDouble / right.y
    val widthFactor = (right.x - left.x + 1).toDouble / y
    println(s"$minAngle <> $maxAngle ~ $widthFactor")

    def leftAt(y: Int): Int =
      val left1 = Int2((minAngle * y).toInt, y)
      val left2 = lookup(left1, L, nonBeamed)
      lookup(left2, R, isBeamed).x
    def rightAt(y: Int): Int =
      val right1 = Int2((maxAngle * y).toInt, y)
      val right2 = lookup(right1, R, nonBeamed)
      lookup(right2, L, isBeamed).x

    enum Fit:
      case Exact(lt: Int2)
      case NoSpace
      case ExtraSpace
    import Fit.*

    def fitCube(y: Int, side: Int): Fit =
      val right = rightAt(y)
      val lt = Int2(right - side + 1, y)
      if nonBeamed(lt) then NoSpace
      else
        val bottom = leftAt(y + side - 1)
        if bottom > lt.x then NoSpace
        else if bottom < lt.x then ExtraSpace
        else Exact(lt)

    val side = 100
    val start = side * (1 + minAngle) * 0.8 / widthFactor
    println(s"Searching from $start...")
    val corner = from(start.toInt).map(fitCube(_, side)).collectFirst {
      case Exact(p) => p
    }.get
    println(corner)
    corner.x * 10_000 + corner.y
