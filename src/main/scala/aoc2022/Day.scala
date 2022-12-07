package aoc2022

trait Day extends common.Day:
  type Input = Iterator[String]
  type Task = Input => Any
  
  def star1Task: Task = ???
  def star2Task: Task = ???

  override def star1(): Any = readInput(star1Task)
  override def star2(): Any = readInput(star2Task)

  final def answerOf(t: Input => Any): Any = readInput(t)