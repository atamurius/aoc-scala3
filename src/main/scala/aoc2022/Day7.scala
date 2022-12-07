package aoc2022

import common.*
import common.read.*

import scala.annotation.tailrec

case object Day7 extends aoc2022.Day:
  case class Dir(path: Seq[String]):
    def / (segment: String): Dir = Dir(path :+ segment)
    def withAllParents: Iterator[Dir] = path.inits.map(Dir.apply)
    def parent: Dir = Dir(path.init)
    override def toString: String = s"/${path mkString "/"}"
  object Dir:
    val Root: Dir = Dir(Nil)

  private object Inputs:
    val CD    = """^\$ cd (.*)$""".r
    val LS    = "$ ls"
    val DIR   = """^dir (.*)$""".r
    val FILE  = """^(\d+) (.*)$""".r

  def parseCommands(input: UndoIterator[String]): Map[Dir, Long] =
    @tailrec def recur(current: Dir, fileSizes: Map[Dir, Long]): Map[Dir, Long] =
      input.nextOption() match
        case None                   => fileSizes
        case Some(Inputs.CD("/"))   => recur(Dir.Root, fileSizes)
        case Some(Inputs.CD(".."))  => recur(current.parent, fileSizes)
        case Some(Inputs.CD(other)) => recur(current / other, fileSizes)
        case Some(Inputs.LS) =>
          val size = input.consumeWhile(!_.startsWith("$")) { ls =>
            ls.map {
              case Inputs.DIR(_)        => 0
              case Inputs.FILE(size, _) => size.toLong
            }.sum
          }
          recur(current, fileSizes.plusAt(current, size))
        case other => sys.error(s"Unexpected input: $other")
    recur(Dir.Root, Map.empty)

  def readRecursiveSize(lines: Iterator[String]): Map[Dir, Long] =
    val filesPerDir = parseCommands(lines.withUndo)
    filesPerDir.foldLeft(Map.empty[Dir, Long]) {
      case (sizes, (dir, size)) =>
        dir.withAllParents.foldLeft(sizes)(_.plusAt(_, size))
    }

  override def star1Task: Task = 
    readRecursiveSize(_).values.filter(_ < 100_000).sum

  override def star2Task: Task = lines =>
    val dirs = readRecursiveSize(lines)
    val spaceFree = 70_000_000 - dirs(Dir.Root)
    val spaceNeeded = 30_000_000
    dirs.values.toVector.sorted.find(_ + spaceFree >= spaceNeeded).get

  override def test(): Unit =
    def t =
      """
        |$ cd /
        |$ ls
        |dir a
        |14848514 b.txt
        |8504156 c.dat
        |dir d
        |dir k
        |$ cd a
        |$ ls
        |dir e
        |29116 f
        |2557 g
        |62596 h.lst
        |$ cd e
        |$ ls
        |584 i
        |$ cd ..
        |$ cd ..
        |$ cd d
        |$ ls
        |4060174 j
        |8033020 d.log
        |5626152 d.ext
        |7214296 k
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 95_437
    answerOf(star1Task) shouldBe 1_582_412
    star2Task(t) shouldBe 24_933_642
    answerOf(star2Task) shouldBe 3_696_336
