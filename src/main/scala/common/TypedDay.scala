package common

trait TypedDay extends common.Day:
  type Input
  type Task = Input => Any
  type Format = Iterable[String] => Input

  def format: Format

  def parseSample(sample: String): Input = format(sample.trim.linesIterator.toVector)

  def star1Task: Task = ???
  def star2Task: Task = ???

  override def star1(): Any = readInput(it => star1Task(format(it to LazyList)))
  override def star2(): Any = readInput(it => star2Task(format(it to LazyList)))

  final def answerOf(t: Input => Any): Any = readInput(it => t(format(it to LazyList)))

object TypedDay:
  trait Generic extends TypedDay:
    override type Input = Iterator[String]
    override def format: Format = _.iterator
