package aoc2021

import common.Day
import common._

case object Day12 extends Day:
  def parse(ls: IterableOnce[String]) =
    ls.iterator
      .map(_.split("-") match { case Array(a,b) => (a,b) })
      .foldLeft(Map.empty[String, Set[String]] withDefaultValue Set.empty) {
        case (acc, (a, b)) =>
          acc + (a -> acc(a).incl(b)) + (b -> acc(b).incl(a))
      }

  val Start = "start"
  val End = "end"

  trait Path[T]:
    def from(cave: String): T
    extension (t: T)
      def current: String
      def navigate(cave: String): Option[T]

  given Path[List[String]] with
    def from(cave: String) = List(cave)
    extension (t: List[String])
      def current: String = t.head
      def navigate(cave: String): Option[List[String]] =
        if isLargeCave(cave) || !t.contains(cave) then Some(cave :: t)
        else None

  case class PathVisitedTwice(current: String, hasTwoSmallCaves: Boolean = false, visited: Set[String] = Set.empty)

  given Path[PathVisitedTwice] with
    def from(cave: String): PathVisitedTwice = PathVisitedTwice(cave, visited = Set(cave))
    extension (t: PathVisitedTwice)
      def current: String = t.current
      def navigate(cave: String): Option[PathVisitedTwice] =
        if isLargeCave(cave) || !t.visited(cave) then Some(t.copy(cave, visited = t.visited + cave))
        else if cave == Start || cave == End || t.hasTwoSmallCaves then None
        else Some(t.copy(cave, t.hasTwoSmallCaves || t.visited(cave), t.visited + cave))

  def isLargeCave(cave: String) = cave.forall(Character.isUpperCase)

  def traverse[T: Path](conn: Map[String, Set[String]]): Iterable[T] =
    Vector(summon[Path[T]].from(Start))
      .converge { pathes =>
        val (ended, pending) = pathes.partition(_.current == End)
        val next = for
          path <- pending
          dest <- conn(path.current)
          res <- path navigate dest
        yield res
        next ++ ended
      }

  override def test(): Unit =
    val t1 =
      """
        |start-A
        |start-b
        |A-c
        |A-b
        |b-d
        |A-end
        |b-end
        |""".stripMargin.trim.linesIterator.toVector
    traverse[List[String]](parse(t1)).size shouldBe 10
    traverse[PathVisitedTwice](parse(t1)).size shouldBe 36
    val t2 =
      """
        |dc-end
        |HN-start
        |start-kj
        |dc-start
        |dc-HN
        |LN-dc
        |HN-end
        |kj-sa
        |kj-HN
        |kj-dc
        |""".stripMargin.trim.linesIterator.toVector
    traverse[List[String]](parse(t2)).size shouldBe 19
    traverse[PathVisitedTwice](parse(t2)).size shouldBe 103
    val t3 =
      """
        |fs-end
        |he-DX
        |fs-he
        |start-DX
        |pj-DX
        |end-zg
        |zg-sl
        |zg-pj
        |pj-he
        |RW-he
        |fs-DX
        |pj-RW
        |zg-RW
        |start-pj
        |he-WI
        |zg-he
        |pj-fs
        |start-RW
        |""".stripMargin.trim.linesIterator.toVector
    traverse[List[String]](parse(t3)).size shouldBe 226
    traverse[PathVisitedTwice](parse(t3)).size shouldBe 3509

  override def star1(): Any = readInput(ls => traverse[List[String]](parse(ls)).size)
  override def star2(): Any = readInput(ls => traverse[PathVisitedTwice](parse(ls)).size)

