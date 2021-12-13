package aoc2021

import common._
import common.read._
import common.coord._

case object Day13 extends Day:
  enum Fold:
    case AlongX(at: Int)
    case AlongY(at: Int)

  def parse(ls: IterableOnce[String]) =
    val it = ls.iterator
    val points = it.takeWhile(_.nonEmpty).map(_.split(",").map(_.toInt).toIterable).map(V[Int2].build).toSet
    val FoldFormat = """fold along (x|y)=(\d+)""".r
    val folds = it.map {
      case FoldFormat("x", value) => Fold.AlongX(value.toInt)
      case FoldFormat("y", value) => Fold.AlongY(value.toInt)
    }
    (points, folds.toVector)

  def fold(ps: Set[Int2], fold: Fold) = fold match
    case Fold.AlongX(x) => ps.map {
      case p if p.x > x => Int2(2*x - p.x, p.y)
      case p => p
    }
    case Fold.AlongY(y) => ps.map {
      case p if p.y > y => Int2(p.x, 2*y - p.y)
      case p => p
    }

  def run: ((Set[Int2], Seq[Fold])) => Set[Int2] = (ps, folds) => folds.foldLeft(ps)(fold)

  override def test(): Unit =
    val sample =
      """
        |6,10
        |0,14
        |9,10
        |0,3
        |10,4
        |4,11
        |6,0
        |6,12
        |4,1
        |0,13
        |10,12
        |3,4
        |3,0
        |8,4
        |1,10
        |2,14
        |8,10
        |9,0
        |
        |fold along y=7
        |fold along x=5
        |""".stripMargin.trim.linesIterator.toVector
    val (ps1, fs1) = parse(sample)
    fold(ps1, fs1.head).size shouldBe 17

  override def star1(): Any =
    val (ps, fs) = readInput(parse)
    fold(ps, fs.head).size

  override def star2(): Any =
    val (ps, fs) = readInput(parse)
    render2d(run(ps, fs))