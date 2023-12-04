package aoc2022

import common.TypedDay
import common.read.Board
import common.coord.*
import common.coord.Dir.*

case object Day23 extends TypedDay.Generic:

  def simulation(lines: Iterator[String]): Iterator[Set[Int2]] =
    val ps = Board.read(lines, "".r).points
      .collect { case (Int2(x, y), "#") => Int2(x, -y) }.toVector

    Iterator.unfold(ps -> Vector(N, S, W, E)) { (ps, dirs) =>
      val occupied = ps.toSet

      def canMoveTo(p: Int2, dir: Dir) =
        val to = p + dir.delta
        !Seq(to, to + dir.left.delta, to + dir.right.delta).exists(occupied)

      val moves = ps.map { p =>
        if !p.adjascent.exists(occupied) then (p, None, p)
        else
          val dir = dirs.find(canMoveTo(p, _))
          (p, dir, dir.fold(p)(p + _.delta))
      }
      val collisions = moves.collect { case (_, Some(_), p) => p }
        .groupBy(identity)
        .filter(_._2.size > 1)

      val movesWithoutCollisions = moves.map {
        case (p, Some(_), dest) if collisions.contains(dest) => (p, None, p)
        case other => other
      }

      if movesWithoutCollisions.exists(_._2.isDefined) then
        val positions = movesWithoutCollisions.map(_._3)
        Some(positions.toSet, (positions, dirs.tail :+ dirs.head))
      else None
    }


  override def star1Task: Task = lines =>
    val res = simulation(lines).drop(9).next()
    val (tl, br) = boundingBox(res)
//    render2d(res.map { case Int2(x,y) => Int2(x,-y) })
    (br.x - tl.x + 1) * (br.y - tl.y + 1) - res.size

  override def star2Task: Task = lines => simulation(lines).size + 1

  override def test(): Unit =
    def t1 =
      """
        |.....
        |..##.
        |..#..
        |.....
        |..##.
        |.....
        |""".stripMargin.trim.linesIterator
    def t2 =
      """
        |....#..
        |..###.#
        |#...#.#
        |.#...##
        |#.###..
        |##.#.##
        |.#..#..
        |""".stripMargin.trim.linesIterator
    star1Task(t2) shouldBe 110
    answerOf(star1Task) shouldBe 3815
    star2Task(t2) shouldBe 20
