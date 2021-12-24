package aoc2021

import common.*

import scala.concurrent.duration.*
import scala.reflect.ClassTag
import scala.util.chaining.*

case object Day24 extends Day:
  sealed trait Arg:
    override def toString: String = this match
      case Var(name) => name
      case Val(v) => v.toString
  case class Var(name: String) extends Arg
  case class Val(value: Int) extends Arg
  enum Instr extends Product:
    case Inp(arg: Var)
    case Add(a: Var, b: Arg)
    case Mul(a: Var, b: Arg)
    case Div(a: Var, b: Arg)
    case Mod(a: Var, b: Arg)
    case Eql(a: Var, b: Arg)

    override def toString: String = s"${productPrefix.toLowerCase} ${productIterator mkString " "}"

  object Instr:
    private val Format = """(\w+) (\w)(?: (\w|-?\d+))?""".r
    private def parseArg(s: String) = if Character.isLetter(s.head) then Var(s) else Val(s.toInt)
    def parse(s: String): Instr = s match
      case Format("inp", arg, _) => Inp(Var(arg))
      case Format("add", a, b) => Add(Var(a), parseArg(b))
      case Format("mul", a, b) => Mul(Var(a), parseArg(b))
      case Format("div", a, b) => Div(Var(a), parseArg(b))
      case Format("mod", a, b) => Mod(Var(a), parseArg(b))
      case Format("eql", a, b) => Eql(Var(a), parseArg(b))

  case class State(input: List[Int], mem: Map[String, Int] = Map.empty[String, Int] withDefaultValue 0):
    def resolve(arg: Arg): Int = arg match
      case Var(name) => mem(name)
      case Val(value) => value

    def update(v: String, value: Int): State = copy(mem = mem.updated(v, value))

    def mapVar(v: String)(f: Int => Int): State = update(v, f(mem(v)))

    def apply: Instr => State =
      case Instr.Inp(Var(v)) => copy(input.tail, mem.updated(v, input.head))
      case Instr.Add(Var(v), arg) => mapVar(v)(_ + resolve(arg))
      case Instr.Mul(Var(v), arg) => mapVar(v)(_ * resolve(arg))
      case Instr.Div(Var(v), arg) => mapVar(v)(_ / resolve(arg))
      case Instr.Mod(Var(v), arg) => mapVar(v)(_ % resolve(arg))
      case Instr.Eql(Var(v), arg) => mapVar(v)(x => if x == resolve(arg) then 1 else 0)

    def run(instr: IterableOnce[Instr], debug: Boolean = false): State =
      if debug then println(this)
      instr.iterator.foldLeft(this) { (s, i) =>
        val next = s.apply(i)
        if debug then
          println()
          println(Color.bright(i))
          println(next)
        next
      }

    override def toString: String =
      val memory = mem.map((n, v) => s"$n = ${Color.blue(v)}").mkString(", ")
      if input.nonEmpty then
        s"$memory; ${Color.yellow("input")} = ${input.mkString(", ")}"
      else
        memory

  enum EvalTree:
    case Ref(name: String)
    case Literal(value: Long)
    case Input(id: Int)
    case Operation(kind: Instr, left: EvalTree, right: EvalTree)

    def writeTo(b: StringBuilder, depthLeft: Int): StringBuilder = this match
      case Literal(v) => b ++= Color.blue(v)
      case Ref(v) => b ++= Color.red(s"$v")
      case Input(id) => b ++= Color.yellow(s"<$id>")
      case Operation(i, l, r) =>
        b ++= "("
        l.writeTo(b, depthLeft - 1) += ' '
        b ++= Color.green(i.productPrefix.toLowerCase) += ' '
        r.writeTo(b, depthLeft - 1) += ')'

    def toString(depth: Int): String = writeTo(new StringBuilder, depth - 1).mkString

  type Vars = Map[String, EvalTree]
  val zero = EvalTree.Literal(0)
  val one = EvalTree.Literal(1)

  def evalTree(instrs: IterableOnce[Instr]): Vars =
    import EvalTree._

    var inputs = -1
    def nextInput = { inputs += 1; Input(inputs) }

    var counter = 0
    var versions = Map.empty[String, Int] withDefaultValue 0
    def latestOf(v: String) = f"${versions(v)}%03d$v"
    def nextOf(v: String) =
      counter += 1
      versions += v -> counter
      f"$counter%03d$v"

    def resolve(a: Arg, tree: Vars) = a match
      case Var(name) =>
        val v = latestOf(name)
        if tree.contains(v) then Ref(v)
        else zero
      case Val(value) => Literal(value)

    instrs.iterator.foldLeft[Vars](Map.empty) {
      case (tree, Instr.Inp(Var(a))) => tree.updated(nextOf(a), nextInput)
      case (tree, k @ Instr.Add(a, b)) =>
        val t = Operation(k, resolve(a, tree), resolve(b, tree))
        tree.updated(nextOf(a.name), t)
      case (tree, k @ Instr.Mul(a, b)) =>
        val t = Operation(k, resolve(a, tree), resolve(b, tree))
        tree.updated(nextOf(a.name), t)
      case (tree, k @ Instr.Div(a, b)) =>
        val t = Operation(k, resolve(a, tree), resolve(b, tree))
        tree.updated(nextOf(a.name), t)
      case (tree, k @ Instr.Mod(a, b)) =>
        val t = Operation(k, resolve(a, tree), resolve(b, tree))
        tree.updated(nextOf(a.name), t)
      case (tree, k @ Instr.Eql(a, b)) =>
        val t = Operation(k, resolve(a, tree), resolve(b, tree))
        tree.updated(nextOf(a.name), t)
    }

  def optimize(vars: Vars, inputs: Map[Int, Int] = Map.empty): Vars =
    import EvalTree._
    val result = vars.keysIterator.max

    def walk(vars: Vars)(f: PartialFunction[EvalTree, EvalTree]): Vars =
      def walkRec(t: EvalTree): EvalTree = f.applyOrElse(t, identity) match
        case Operation(o, l, r) => Operation(o, walkRec(l), walkRec(r))
        case other => other
      vars.transform((_, t) => walkRec(t))

    def countUsages(v: String, vs: Vars): Int =
      def count(ts: Iterator[EvalTree]): Int = ts.map {
        case _ if v == result => 2
        case Ref(`v`) => 1
        case Operation(_, l, r) => count(Iterator(l, r))
        case _ => 0
      }.sum
      count(vs.valuesIterator)

    def inlineSingletons(vars: Vars): Vars = walk(vars) {
      case t @ Operation(_, Ref(v), _) if countUsages(v, vars) == 1 => t.copy(left = vars(v))
      case t @ Operation(_, _, Ref(v)) if countUsages(v, vars) == 1 => t.copy(right = vars(v))
      case Ref(v) if vars.get(v).exists(t => t.isInstanceOf[Input] || t.isInstanceOf[Literal]) => vars(v)
      case Input(id) if inputs.contains(id) => Literal(inputs(id))
    }

    def eliminateUnused(vars: Vars): Vars =
      vars.keysIterator.foldLeft(vars) { (tree, variable) =>
        val usages = countUsages(variable, tree)
        if usages > 0 then tree
        else tree - variable
      }

    object O:
      class Matcher[I <: Instr: ClassTag]:
        def unapply(t: EvalTree): Option[(EvalTree, EvalTree)] = t match
          case Operation(_: I, l, r) => Some(l, r)
          case _ => None
      object add extends Matcher[Instr.Add]
      object mul extends Matcher[Instr.Mul]
      object div extends Matcher[Instr.Div]
      object mod extends Matcher[Instr.Mod]
      object eql extends Matcher[Instr.Eql]

    def evalKnown(vars: Vars): Vars = walk(vars) {
      case O.add(`zero`, x) => x
      case O.add(x, `zero`) => x
      case O.mul(`one`, x) => x
      case O.mul(x, `one`) => x
      case O.mul(`zero`, _) => zero
      case O.mul(_, `zero`) => zero
      case O.div(`zero`, _) => zero
      case O.mod(`zero`, _) => zero
      case O.div(x, `one`) => x
      case O.mod(_, `one`) => zero
      case O.eql(Literal(a), Literal(b)) => Literal(if a == b then 1 else 0)
      case O.add(Literal(a), Literal(b)) => Literal(a + b)
      case O.mul(Literal(a), Literal(b)) => Literal(a * b)
      case O.div(Literal(a), Literal(b)) => Literal(a / b)
      case O.mod(Literal(a), Literal(b)) => Literal(a % b)

      // (? mod a) + b =?= <input>
      case O.eql(O.add(O.mod(_, Literal(a)), Literal(b)), Input(_))
        // b .. a + b - 1 ~ 1 .. 9
        if b > 9 || (b + a - 1) < 1 => Literal(0)
    }

    vars.converge { vars =>
      vars
        .pipe(inlineSingletons)
        .pipe(evalKnown)
        .pipe(if inputs.isEmpty then eliminateUnused else identity)
    }

  override def test(): Unit =
    val sample =
      """
        |inp w
        |add z w
        |mod z 2
        |div w 2
        |add y w
        |mod y 2
        |div w 2
        |add x w
        |mod x 2
        |div w 2
        |mod w 2
        |""".stripMargin.trim.linesIterator.map(Instr.parse).toVector
    State(List(15)).run(sample).mem shouldBe Map("x" -> 1, "y" -> 1, "z" -> 1, "w" -> 1)

  override def star1(): Any =
    val program = readInput(_.map(Instr.parse).toVector)
    val tree = optimize(evalTree(program))
    val inputs = Map[Int, Int](
      0 -> 2,
      1 -> 4,
      2 -> 9,
      3 -> 1, // $2 - 8
      4 -> 3,
      5 -> 1, // $4 - 2
      6 -> 1, // $1 - 3
      7 -> 1,
      8 -> 6,
      9 -> 1,
      10 -> 6, // $9 + 5
      11 -> 1, // $8 - 5
      12 -> 5, // $7 + 4
      13 -> 1, // $0 - 1
    )
    val code = inputs.toList.sortBy(_._1).map(_._2)
    State(code).run(program).mem("z") shouldBe 0
    val result = optimize(tree, inputs)
    for (v, t) <- tree.toVector.sortBy(_._1) do
      val resulted = result.get(v).filter(_ != t)
      println(s"${Color.red(v)} = ${t.toString(5)} -> ${resulted.fold("-")(_.toString(5))}")
    code.mkString
