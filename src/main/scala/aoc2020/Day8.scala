package aoc2020

import scala.annotation.tailrec

case object Day8 extends Day:

  enum Instruction:
    case Acc(amount: Int)
    case Jmp(instructions: Int)
    case Nop(notUsed: Int)
  
  object Instruction:
    private val Format = """(acc|jmp|nop) \+?(-?\d+)""".r
    def parse(line: String): Instruction = line match
      case Format("acc", n) => Acc(n.toInt)
      case Format("jmp", n) => Jmp(n.toInt)
      case Format("nop", n) => Nop(n.toInt)
      case other => sys.error(s"Unknown instruction: $other")

  case class Program(code: Vector[Instruction], acc: Int = 0, pointer: Int = 0):
    def next: Program = code(pointer) match
      case Instruction.Acc(amount) => copy(acc = acc + amount, pointer = pointer + 1)
      case Instruction.Jmp(delta) => copy(pointer = pointer + delta)
      case Instruction.Nop(_) => copy(pointer = pointer + 1)

    def isHalted: Boolean = pointer == code.size
    
    def after(steps: Int): Program = if steps == 0 then this else next after (steps - 1) 
  
    def runToLoop: Program =
      @tailrec def run(current: Program, executed: Set[Int]): Program =
        if executed(current.pointer) || current.isHalted then current
        else run(current.next, executed + current.pointer)
      run(this, Set.empty)
  
    def swapAt(pos: Int): Program = 
      val swapped = code(pos) match
        case Instruction.Jmp(x) => Instruction.Nop(x)
        case Instruction.Nop(x) => Instruction.Jmp(x)
        case other => other
      copy(code = code.updated(pos, swapped))
  
    def findHalted: Option[Program] = (
      for 
        pos <- code.indices.iterator if !code(pos).isInstanceOf[Instruction.Acc]
        result = swapAt(pos).runToLoop if result.isHalted
      yield result
    ).toSeq.headOption
  
  def parse(lines: Iterator[String]) = Program(lines.map(Instruction.parse).toVector)
  
  override def test(): Unit =
    val sample = parse(
      """
        |nop +0
        |acc +1
        |jmp +4
        |acc +3
        |jmp -3
        |acc -99
        |acc +1
        |jmp -4
        |acc +6
        |""".stripMargin.trim.linesIterator)
  
    val test1 = sample.runToLoop
    test1.isHalted shouldBe false
    test1.pointer shouldBe 1
    test1.acc shouldBe 5
  
    val test2 = sample.swapAt(7).runToLoop
    test2.isHalted shouldBe true
    test2.acc shouldBe 8
    sample.findHalted shouldBe Some(test2)

  override def star1(): Any = readInput(parse).runToLoop.acc

  override def star2(): Any = readInput(parse).findHalted.map(_.acc)
