package aoc2019

import common.{Color, Day}
import IntCode.*
import common.coord.*
import common.read.Board

import scala.annotation.tailrec
import scala.concurrent.duration.*

case object Day17 extends Day:
  def readBoard(machine: Machine = machine(readInput(_.next()))): Board[String] =
    Board.read(
      machine
        .runIO()
        .map(_.toChar)
        .mkString
        .linesIterator,
      "".r)

  def intersections(board: Board[String]): Iterator[Int2] =
    board.points.collect {
      case (p, "#") if
        board.get(p.neighbours).forall(_ == "#") &&
          board.get(p.adjascent).count(_ == "#") == 4
        => p
    }

  enum Instr:
    case TurnLeft
    case TurnRight
    case Move(steps: Int)

    override def toString: String = this match
      case Instr.TurnRight => "R"
      case Instr.TurnLeft => "L"
      case Instr.Move(x) => x.toString

  def formatInstr(instr: Seq[Instr]): String =
    val buff = new StringBuffer()
    for x <- instr do
      if buff.length > 0 then buff append ','
      buff.append(x)
    buff.toString

  def traversePath(board: Board[String]): Seq[Instr] =
    val Some(start, dir) = board.points.collectFirst {
      case (p, "<") => (p, GridDir.L)
      case (p, ">") => (p, GridDir.R)
      case (p, "v") => (p, GridDir.D)
      case (p, "^") => (p, GridDir.U)
    }
    @tailrec def go(pos: Int2, dir: Dir, path: Vector[Instr]): Vector[Instr] =
      val possibleDirs =
        Seq(Instr.TurnLeft -> dir.right, Instr.TurnRight -> dir.left)
          .find((_, dir) => board.get(pos + dir.delta) contains "#")
      if possibleDirs.isEmpty then path
      else
        val (turn, nextDir) = possibleDirs.head
        val steps = Iterator.iterate(pos)(_ + nextDir.delta)
          .drop(1).takeWhile(p => board.get(p).contains("#"))
          .toVector
        val moves = Seq(Instr.Move(steps.size))
        go(steps.last, nextDir, path ++ Seq(turn) ++ moves)
    go(start, dir, Vector.empty)

  @tailrec
  def includes[T](all: Seq[T], sub: Seq[T], from: Int = 0, res: Seq[Int] = Vector.empty): Seq[Int] =
    all.indexOfSlice(sub, from) match
      case -1 => res
      case  i => includes(all, sub, i + 1, res :+ i)

  def buildFrom[T](target: Seq[T], chunks: Seq[Seq[T]], offset: Int = 0): Seq[List[Int]] =
    if offset == target.size then Seq(Nil)
    else chunks.zipWithIndex.flatMap { (chunk, i) =>
      if target.slice(offset, offset + chunk.size) != chunk then Nil
      else
        buildFrom(target, chunks, offset + chunk.size).map(i :: _)
    }

  def divide(path: Seq[Instr]): Iterator[(Seq[Int], Seq[Seq[Instr]])] =
    def reasonableChunks(from: Int): Iterator[Seq[Instr]] =
      Iterator.range(from + 2, path.size)
        .map(path.slice(from, _))
        .takeWhile(formatInstr(_).length < 20)
    for
      a     <- reasonableChunks(0)
      bFrom <- a.size until path.size
      b     <- reasonableChunks(bFrom)
      cFrom <- bFrom + b.size until path.size
      c     <- reasonableChunks(cFrom)
      chunks = Seq(a, b, c)
      comp  <- buildFrom(path, chunks) if comp.size <= 10
    yield (comp, chunks)

  def optimizePath(board: Board[String]): Unit =
    board.render((_, s) => s" $s")

    val path = traversePath(board)
    println(formatInstr(path))
    divide(path).nextOption match
      case None => println("No solution")
      case Some(comp, cs) =>
        println(s"Possible division: ${comp mkString ","}")
        for (c, i) <- cs.zipWithIndex do
          val str = formatInstr(c)
          println(s"$i: $str (${str.length})")

  override val timeout: FiniteDuration = 500.seconds

  override def test(): Unit =
    val sample = Board.read(
      """
        |..#..........
        |..#..........
        |#######...###
        |#.#...#...#.#
        |#############
        |..#...#...#..
        |..#####...^..
        |""".stripMargin.trim.linesIterator,
      "".r
    )
    intersections(sample).map(_.components.product).sum shouldBe 76

  override def star1(): Any = intersections(readBoard()).map(_.components.product).sum

  override def star2(): Any =
    val m = machine(readInput(_.next()))
    val board = readBoard(m)
    val Some(seq, chunks) = divide(traversePath(board)).nextOption()
    val input =
      seq.map(i => (i + 'A').toChar).mkString(",") + "\n" +
        chunks.map(c => formatInstr(c) + "\n").mkString +
        "n\n"
    println(input)
    val output = m.write(0 -> 2).runIO(input.map(_.toLong)*)
    println(output.view.dropRight(1).map(_.toChar).mkString)
    output.lastOption
