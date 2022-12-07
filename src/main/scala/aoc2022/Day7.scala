package aoc2022

import common.*

case object Day7 extends aoc2022.Day:
  type Dir = Seq[String]
  val Root = Nil

  def readRecursiveSize(lines: Iterator[String]): Map[Dir, Long] =
    var dirs = Map.empty[Seq[String], Map[String, Long]]
    var current: Seq[String] = Root
    val CD = """^\$ cd (.*)$""".r
    val DIR = """^dir (.*)$""".r
    val FILE = """^(\d+) (.*)$""".r
    lines.foreach {
      case CD("/")        => current = Root
      case CD("..")       => current = current.init
      case CD(dir)        => current :+= dir
      case "$ ls"         =>
      case DIR(dir)       => dirs += (current :+ dir) -> Map.empty
      case FILE(size, f)  =>
        dirs = dirs.putMerge(current, Map(f -> size.toLong))(_ ++ _)
    }
    var recursiveSize = Map.empty[Seq[String], Long]
    for (d, fs) <- dirs; dir <- d.inits do
      recursiveSize = recursiveSize.putMerge(dir, fs.values.sum)(_ + _)
    recursiveSize

  override def star1Task = readRecursiveSize(_).values.filter(_ < 100_000).sum

  override def star2Task = lines =>
    val dirs = readRecursiveSize(lines)
    val spaceFree = 70_000_000 - dirs(Root)
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
