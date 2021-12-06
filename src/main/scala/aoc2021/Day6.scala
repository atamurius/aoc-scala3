package aoc2021

import common.Day

case object Day6 extends Day :
  type Age = Int
  val InitialAge = 8
  val ResetAge   = 6
  val ReadyAge   = 0

  case class School(ages: Map[Age, Long]) extends AnyVal :
    override def toString: String = ages.iterator.flatMap((a, n) => (1 to n.toInt).map(_ => a)).mkString(",")

    def size = ages.valuesIterator.sum

    def tick = School(ages
      .iterator
      .flatMap {
        case (ReadyAge, n) => (InitialAge, n) :: (ResetAge, n) :: Nil
        case (age, n) => (age - 1, n) :: Nil
      }
      .foldLeft(Map.empty[Age, Long] withDefaultValue 0L) {
        case (acc, (x, n)) => acc + (x -> (acc(x) + n))
      }
    )

    def ticks = Iterator.iterate(this)(_.tick)

  def parse(line: String) = School(line.split(",").map(_.toInt).groupBy(identity).transform((_, ns) => ns.length))

  override def test(): Unit =
    val sample = parse("""3,4,3,1,2""")
    sample.ticks.drop(18).next().size shouldBe 26L
    sample.ticks.drop(80).next().size shouldBe 5934L
    sample.ticks.drop(256).next().size shouldBe 26984457539L

  override def star1(): Any = readInput(it => parse(it.next())).ticks.drop(80).next().size

  override def star2(): Any = readInput(it => parse(it.next())).ticks.drop(256).next().size
