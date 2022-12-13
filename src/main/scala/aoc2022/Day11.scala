package aoc2022

import common.*
import common.read.*
import common.parse.*

import scala.concurrent.duration._

case object Day11 extends aoc2022.Day:
  type MonkeyId = String
  type Item = Long
  def Item(x: String) = x.toLong
  case class Monkey(
    items: Seq[Item],
    operation: Item => Item,
    divisor: Int,
    successMonkey: MonkeyId,
    failureMonkey: MonkeyId,
    inspectedCount: Long = 0,
  ):
    def test(item: Item): MonkeyId = if item % divisor == 0 then successMonkey else failureMonkey
    def addItem(item: Item) = copy(items = items :+ item)
    def inspected() = copy(items = Vector.empty, inspectedCount = inspectedCount + items.size)

  val monkeysFormat: lines.Format[Map[MonkeyId, Monkey]] = {
    val id = line("Monkey " *> chunkUntil(':').asString <* ":")
    val monkey = for
      items     <- line("  Starting items: " *> numberAs[Item].delimitedBy(", "))
      operation <- line(
        "  Operation: new = old " *> ("[+*]".r <* " ") <*> ("old" or numberAs[Item]) mapWithError {
          case ("+", arg) => Right((old: Item) => old + arg.getOrElse(old))
          case ("*", arg) => Right((old: Item) => old * arg.getOrElse(old))
          case (other, _) => Left(s"Unknown operation $other")
        }
      )
      test      <- line("  Test: divisible by " *> numberAs[Int])
      ifTrue    <- line("    If true: throw to monkey " *> line.any.asString)
      ifFalse   <- line("    If false: throw to monkey " *> line.any.asString)
    yield Monkey(items, operation, test, ifTrue, ifFalse)

    (id <*> monkey).delimitedBy(lines.blank).map(_.toMap)
  }

  override def star1Task: Task = lines => playWith(monkeysFormat(lines.toSeq), 20, _ / 3)
  override def star2Task: Task = lines =>
    val monkeys = monkeysFormat(lines.toSeq)
    val p = monkeys.values.map(_.divisor).product
    playWith(monkeys, 10_000, _ % p)

  def playWith(monkeys: Map[MonkeyId, Monkey], rounds: Int, update: Item => Item) =
    val result = (1 to rounds).foldLeft(monkeys) { (state, round) =>
      val monkeys = collection.mutable.Map.from(state)
      for (currentId, current) <- monkeys do
        for item <- current.items do
          val updated = update(current.operation(item))
          val to = current.test(updated)
          monkeys.updateWith(to)(_.map(_.addItem(updated)))
        monkeys.updateWith(currentId)(_.map(_.inspected()))
      monkeys.toMap
    }
    result.values.map(_.inspectedCount).toVector.sorted.takeRight(2).product

  override def test(): Unit =
    def t =
      """
        |Monkey 0:
        |  Starting items: 79, 98
        |  Operation: new = old * 19
        |  Test: divisible by 23
        |    If true: throw to monkey 2
        |    If false: throw to monkey 3
        |
        |Monkey 1:
        |  Starting items: 54, 65, 75, 74
        |  Operation: new = old + 6
        |  Test: divisible by 19
        |    If true: throw to monkey 2
        |    If false: throw to monkey 0
        |
        |Monkey 2:
        |  Starting items: 79, 60, 97
        |  Operation: new = old * old
        |  Test: divisible by 13
        |    If true: throw to monkey 1
        |    If false: throw to monkey 3
        |
        |Monkey 3:
        |  Starting items: 74
        |  Operation: new = old + 3
        |  Test: divisible by 17
        |    If true: throw to monkey 0
        |    If false: throw to monkey 1
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 10605
    answerOf(star1Task) shouldBe 66124
    star2Task(t) shouldBe 2_713_310_158L
    answerOf(star2Task) shouldBe 19_309_892_877L
