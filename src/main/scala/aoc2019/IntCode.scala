package aoc2019

import common.*

import scala.Specializable.Args
import scala.collection.Iterator.iterate

object IntCode:

  object IO extends StatefulIO[Machine]
  import IO.*

  type Addr = Int

  object Read:
    val pointer: IO[Addr] = state.map(_.pointer)
    def at(addr: Addr): IO[Int] = state.map(_.code(addr))

  object Change:
    def apply(f: Machine => Machine): IO[Unit] = state.map(f).flatMap(replace)
    def jump(p: Addr): IO[Unit] = Change(_.copy(pointer = p))
    def jumpRel(delta: Int): IO[Unit] = Change(_.changePointer(_ + delta))
    val readNext: IO[Int] = Read.pointer <* jumpRel(1) flatMap Read.at
    def writeAt(addr: Addr, value: Int): IO[Unit] = Change(_.changeCode(_.updated(addr, value)))

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

  object OpCode:
    def parse(code: Int): OpCode = values.find(_.code == code % 100).getOrElse(sys.error(s"Unknown opcode $code"))

    private def binaryOp(op: (Int, Int) => Int): IO[State] =
      for case a :: b :: Nil <- Arg.read(2)
          _ <- Arg.write(op(a, b))
      yield State.Continue

    def executeCurrent: IO[State] = Read.pointer flatMap Read.at map parse flatMap execute

    def execute: OpCode => IO[State] =
      case OpCode.Hlt => pure(State.Halted)
      case OpCode.Add => binaryOp(_ + _)
      case OpCode.Mul => binaryOp(_ * _)
      case OpCode.Inp => Arg.readNone *> Change.readNext.map(State.Input(_))
      case OpCode.Out => Arg.readOne.map(State.Output(_))
      case OpCode.Jnz =>
        Arg.read(2).flatMap {
          case x :: addr :: Nil if x != 0 => Change.jump(addr).as(State.Continue)
          case _ => pure(State.Continue)
        }
      case OpCode.Jiz =>
        Arg.read(2).flatMap {
          case 0 :: addr :: Nil => Change.jump(addr).as(State.Continue)
          case _ => pure(State.Continue)
        }
      case OpCode.Ltn => binaryOp { (a, b) => if a < b then 1 else 0 }
      case OpCode.Eql => binaryOp { (a, b) => if a == b then 1 else 0 }

  enum State:
    case Continue
    case Halted
    case Input(address: Addr)
    case Output(value: Int)

  type Program = Vector[Int]

  enum Arg:
    case Position
    case Immediate

  object Arg:
    def read(n: Int): IO[List[Int]] =
      Change.readNext.flatMap { code =>
        iterate(code)(_ / 10).map(_ % 10).drop(2).map(Arg.fromOrdinal)
          .take(n).toList.traverse {
            case Arg.Position  => Change.readNext >>= Read.at
            case Arg.Immediate => Change.readNext
          }
      }
    def readNone: IO[Unit] = read(0).void
    def readOne: IO[Int] = read(1).map(_.head)

    def write(x: Int): IO[Unit] = Change.readNext >>= (Change.writeAt(_, x))

  case class Machine(code: Program, pointer: Addr = 0):
    def changeCode(f: Program => Program) = copy(f(code))
    def changePointer(f: Addr => Addr) = copy(pointer = f(pointer))
    def write(addrValue: (Addr, Int)*) = copy(addrValue.foldLeft(code)(_.updated.tupled(_)))
    override def toString: String = s"[$pointer] ${code mkString ","}"
    def run: Machine = Machine.run(this)._2
    def runIO(input: Int*): Vector[Int] = Machine.runIO(input.toList)(this)._1
    def withInput(p: Int) = Machine.runUntil { case State.Input(addr) => Change.writeAt(addr, p) }(this)._2
    def runToOutput = Machine.runUntil { case State.Output(x) => pure(x) }(this)

  def machine(program: String) = Machine(program.split(",").toVector.map(_.toInt))

  object Machine:
    def runUntil[T](pf: PartialFunction[State, IO[T]]): IO[T] = OpCode.executeCurrent.flatMap {
      case State.Continue                 => runUntil(pf)
      case other if pf.isDefinedAt(other) => pf(other)
      case unexpected                     => sys.error(s"Unexpected state: $unexpected")
    }
    val run: IO[Unit] = runUntil { case State.Halted => pure(()) }
    def runIO(input: List[Int], output: Vector[Int] = Vector.empty): IO[Vector[Int]] =
      runUntil {
        case State.Input(addr) => input match
          case Nil => sys.error("Empty input")
          case x :: rest => Change.writeAt(addr, x) *> runIO(rest, output)

        case State.Output(x) => runIO(input, output :+ x)
        case State.Halted => pure(output)
      }
