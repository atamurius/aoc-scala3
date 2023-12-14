package aoc2019

import aoc2019.IntCode.Machine.{runToInput, runUntilInterruption}
import common.*

import scala.Specializable.Args
import scala.collection.Iterator.iterate
import scala.collection.immutable.SortedMap

object IntCode:

  object IO extends StatefulIO[Machine]
  import IO.*

  type Addr = Int
  type Val = Long
  def asAddr(v: Val): Addr = v.toInt match
    case x if x < 0 => sys.error(s"Invalid address: $v")
    case x => x

  object Read:
    val pointer: IO[Addr] = state.map(_.pointer)
    def at(addr: Addr): IO[Val] = state.map(_.code.getOrElse(addr, 0))
    def atRelative(addr: Val): IO[Val] = state.map(m => m.code.getOrElse(asAddr(addr + m.relativeBase), 0))
    def relativeBase: IO[Addr] = state.map(_.relativeBase)

  object Change:
    def apply(f: Machine => Machine): IO[Unit] = state.map(f).flatMap(replace)
    def jump(p: Addr): IO[Unit] = Change(_.copy(pointer = p))
    def jumpRel(delta: Int): IO[Unit] = Change(_.changePointer(_ + delta))
    val readNext: IO[Val] = Read.pointer <* jumpRel(1) flatMap Read.at
    def writeAt(addr: Addr, value: Val): IO[Unit] = Change(_.changeCode(_.updated(addr, value)))

  enum OpCode(val code: Int):
    case Hlt extends OpCode(99)
    case Add extends OpCode(1)
    case Mul extends OpCode(2)
    case Inp extends OpCode(3)
    case Out extends OpCode(4)
    case Jnz extends OpCode(5)
    case Jiz extends OpCode(6)
    case Ltn extends OpCode(7)
    case Eql extends OpCode(8)
    case Arb extends OpCode(9)

  object OpCode:
    def parse(code: Val): OpCode = values.find(_.code == code % 100).getOrElse(sys.error(s"Unknown opcode $code"))

    private def binaryOp(op: (Val, Val) => Val): IO[State] =
      for case a :: b :: c :: Nil <- Arg(3)
          a <- a.read()
          b <- b.read()
          _ <- c.write(op(a, b))
      yield State.Continue

    def current: IO[OpCode] = Read.pointer flatMap Read.at map parse

    def executeCurrent: IO[State] = current flatMap execute

    def execute: OpCode => IO[State] =
      case OpCode.Hlt => pure(State.Halted)
      case OpCode.Add => binaryOp(_ + _)
      case OpCode.Mul => binaryOp(_ * _)
      case OpCode.Inp => Arg.single.flatMap(_.readAddr()).map(State.Input(_))
      case OpCode.Out => Arg.singleIn.map(State.Output(_))
      case OpCode.Jnz =>
        for case x :: addr :: Nil <- Arg(2)
          x <- x.read()
          _ <- (addr.read().map(asAddr) >>= Change.jump).when(x != 0)
          yield State.Continue
      case OpCode.Jiz =>
      for case x :: addr :: Nil <- Arg(2)
          x <- x.read()
          _ <- (addr.read().map(asAddr) >>= Change.jump).when(x == 0)
      yield State.Continue
      case OpCode.Ltn => binaryOp { (a, b) => if a < b then 1 else 0 }
      case OpCode.Eql => binaryOp { (a, b) => if a == b then 1 else 0 }
      case OpCode.Arb => Arg.singleIn.flatMap { x =>
        Change(m => m.copy(relativeBase = (m.relativeBase + x).toInt)).as(State.Continue)
      }

  enum State:
    case Continue
    case Halted
    case Input(address: Addr)
    case Output(value: Val)

  type Program = collection.immutable.SortedMap[Addr, Val]

  enum ArgType:
    case Position
    case Immediate
    case Relative

  case class Arg(value: Val, argType: ArgType):
    def read(): IO[Val] = argType match
      case ArgType.Immediate => IO.pure(value)
      case ArgType.Position  => Read.at(asAddr(value))
      case ArgType.Relative  => Read.atRelative(value)

    def readAddr(): IO[Addr] = argType match
      case ArgType.Relative => Read.relativeBase.map(_ + value).map(asAddr)
      case _                => IO.pure(asAddr(value))

    def write(x: Val): IO[Unit] = readAddr() >>= (Change.writeAt(_, x))

  object Arg:
    def apply(n: Int): IO[List[Arg]] =
      Change.readNext.flatMap { code =>
        Iterator.iterate(code)(_ / 10).map(_.toInt % 10).drop(2).map(ArgType.fromOrdinal)
          .take(n).toList.traverse { argType =>
            Change.readNext map (Arg(_, argType))
          }
      }
    def none: IO[Unit] = apply(0).void
    def single: IO[Arg] = apply(1).map(_.head)
    def singleIn: IO[Val] = apply(1).flatMap(_.head.read())

  case class Machine(code: Program, pointer: Addr = 0, relativeBase: Addr = 0):
    def changeCode(f: Program => Program): Machine = copy(f(code))
    def changePointer(f: Addr => Addr): Machine = copy(pointer = f(pointer))
    def write(addrValue: (Addr, Val)*): Machine = copy(addrValue.foldLeft(code)(_.updated.tupled(_)))
    def describeMemory: String =
      def memoryChunk(offset: Int): String =
        val chunk = Iterator.from(offset).takeWhile(code.contains).map(code(_)).toVector
        if chunk.isEmpty then ""
        else
          val nextAddresses = code.keySet.rangeFrom(offset + chunk.size)
          val next = if nextAddresses.isEmpty then "" else memoryChunk(nextAddresses.firstKey)
          val prefix = if offset > 0 then s"...$offset: " else ""
          s"$prefix${chunk mkString ","}$next"
      memoryChunk(0)
    override def toString: String = s"[$pointer] $describeMemory"
    def isTerminated: Boolean = OpCode.current.map(_ == OpCode.Hlt)(this)._1

    def eval[T](op: IO[T]): T = op(this)._1
    def stateAfter(op: IO[_]): Machine = op(this)._2

    def run: Machine = stateAfter(Machine.run)
    def runIO(input: Val*): Vector[Val] = eval(Machine.runIO(input.toList))
    def runAsciiIO(input: String): Vector[Val] = eval(Machine.runIO(input.map(x => x: Val).toList))
    def withInput(p: Val): Machine = stateAfter(Machine.runToInput(p))
    def runToOutput: (Val, Machine) = Machine.runToOutput(this)
    def runInteractive(): Machine = stateAfter(Machine.runInteractive())

  def machine(program: String): Machine =
    Machine(SortedMap(program.split(",").toVector.map(_.toLong).zipWithIndex.map((a, b) => (b, a)): _*))

  object Machine:
    def runUntil[T](pf: PartialFunction[State, IO[T]]): IO[T] = OpCode.executeCurrent.repeatUntil {
      case State.Continue                 => None
      case other if pf.isDefinedAt(other) => Some(pf(other))
      case unexpected                     => sys.error(s"Unexpected state: $unexpected")
    }
    def runUntilInterruption: IO[State] = runUntil { case any => pure(any) }
    val run: IO[Unit] = runUntil { case State.Halted => pure(()) }
    def runIO(input: List[Val], output: Vector[Val] = Vector.empty): IO[Vector[Val]] =
      runUntil {
        case State.Input(addr) => input match
          case Nil => sys.error("Empty input")
          case x :: rest => Change.writeAt(addr, x) *> runIO(rest, output)

        case State.Output(x) => runIO(input, output :+ x)
        case State.Halted => pure(output)
      }
    def runInteractive(inputBuffer: List[Val] = Nil): IO[Unit] =
      runUntil {
        case State.Input(addr) =>
          val input = inputBuffer match
          case Nil =>
            Console.print("> ")
            val line = Console.in.readLine()
            (line.map(x => x: Val) :+ 10L).toList
          case something => something
          Change.writeAt(addr, input.head) *> runInteractive(input.tail)

        case State.Output(x) =>
          if x >= 0 && x <= 0xff then
            Console.print(x.toChar)
          else
            Console.println(s"OUT: $x")
          runInteractive(inputBuffer)

        case State.Halted => pure(())
      }
    def runToInput(x: Val): IO[Unit] = runUntil { case State.Input(addr) => Change.writeAt(addr, x) }
    def runToOutput: IO[Val] = runUntil { case State.Output(x) => pure(x) }
