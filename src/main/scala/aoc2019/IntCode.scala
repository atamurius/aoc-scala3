package aoc2019

import common.*

object IntCode:

  object IO extends StatefulIO[Machine]
  import IO.*

  object Read:
    val pointer: IO[Int] = state.map(_.pointer)
    def at(addr: Int): IO[Int] = state.map(_.code(addr))
    def currentOp: IO[OpCode] = pointer.flatMap(at).map(OpCode.parse)

  object Change:
    def apply(f: Machine => Machine): IO[Unit] = state.map(f).flatMap(replace)
    def jump(p: Int): IO[Unit] = Change(_.copy(pointer = p))
    def jumpRel(delta: Int): IO[Unit] = Change(_.changePointer(_ + delta))
    val readNext: IO[Int] = Read.pointer <* jumpRel(1) flatMap Read.at
    def writeAt(addr: Int, value: Int): IO[Unit] = Change(_.changeCode(_.updated(addr, value)))

  type Program = Vector[Int]

  case class Machine(code: Program, pointer: Int = 0):
    def changeCode(f: Program => Program) = copy(f(code))
    def changePointer(f: Int => Int) = copy(pointer = f(pointer))
    def write(addrValue: (Int, Int)*) = copy(addrValue.foldLeft(code)(_.updated.tupled(_)))
    override def toString: String = s"[$pointer] ${code mkString ","}"
    def run: Machine = Machine.run(this)._2

  object Machine:
    enum State:
      case Ok
      case Halted

    def binaryOp(op: (Int, Int) => Int): IO[State] =
      for _ <- Change.readNext
          a <- Change.readNext flatMap Read.at
          b <- Change.readNext flatMap Read.at
          _ <- Change.readNext flatMap (Change.writeAt(_, op(a, b)))
      yield State.Ok

    val step: IO[State] = Read.currentOp.flatMap {
      case OpCode.Halt => pure(State.Halted)
      case OpCode.Add  => binaryOp(_ + _)
      case OpCode.Mul  => binaryOp(_ * _)
    }
    val run: IO[Unit] = step.flatMap {
      case State.Halted => pure(())
      case State.Ok     => run
    }

  enum OpCode(val code: Int):
    case Halt extends OpCode(99)
    case Add  extends OpCode(1)
    case Mul  extends OpCode(2)

  object OpCode:
    def parse(code: Int) = values.find(_.code == code).getOrElse(sys.error(s"Unknown opcode $code"))

  def machine(program: String) = Machine(program.split(",").toVector.map(_.toInt))
