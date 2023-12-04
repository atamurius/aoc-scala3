package aoc2022

import common.TypedDay
import common.parse.*

case object Day21 extends TypedDay.Generic:
  enum Expr:
    case Var(id: String)
    case Val(value: Long)
    case Op(left: Expr, op: Char, right: Expr)

    def eval(ctx: Map[Expr, Expr]): Long = this match
      case Val(x)               => x
      case Var(_)               => ctx(this).eval(ctx)
      case Op(left, '+', right) => left.eval(ctx) + right.eval(ctx)
      case Op(left, '-', right) => left.eval(ctx) - right.eval(ctx)
      case Op(left, '*', right) => left.eval(ctx) * right.eval(ctx)
      case Op(left, '/', right) => left.eval(ctx) / right.eval(ctx)

    def resolve(ctx: Map[Expr, Expr]): Expr = this match
      case Val(_) => this
      case Var(_) => ctx.get(this).fold(this)(_.resolve(ctx))
      case Op(left, op, right) => Op(left.resolve(ctx), op, right.resolve(ctx)) match
        case op @ Op(Val(_), _, Val(_)) => Val(op.eval(ctx))
        case Op(left, op @ ('+'|'*'), right @ Val(_)) => Op(right, op, left)
        case op => op



  import Expr.*

  val root = Var("root")

  val parseInput: lines.Format[Map[Expr, Expr]] = {
    val id = pattern("\\w+".r).map(Var(_))
    val num = numberAs[Long].map(Val(_))
    val op = (id *: (" " *> line.single <* " " <*> id)).map {
      case (a, op, b) => Op(a, op, b)
    }
    val expression = id <* ": " <*> (num or op).map(_.fold(identity, identity))
    line(expression).repeated.map(_.toMap)
  }

  override def star1Task: Task = lines =>
    val ctx = parseInput(lines.toVector)
    root.eval(ctx)

  override def star2Task: Task = lines =>
    val ctx = parseInput(lines.toVector) - Var("humn")
    val Op(a, _, b) = ctx(root)

    def eval(expr: Expr, value: Long): Long = expr match
      case Var(_) => value
      case Op(Val(x), '+', expr) => eval(expr, value - x)
      case Op(Val(x), '*', expr) => eval(expr, value / x)
      case Op(Val(x), '-', expr) => eval(expr, x - value)
      case Op(Val(x), '/', expr) => eval(expr, x / value)
      case Op(expr, '-', Val(x)) => eval(expr, x + value)
      case Op(expr, '/', Val(x)) => eval(expr, x * value)

    (a.resolve(ctx), b.resolve(ctx)) match
      case (Val(left), right) => eval(right, left)
      case (left, Val(right)) => eval(left, right)

  override def test(): Unit =
    def t =
      """root: pppw + sjmn
        |dbpl: 5
        |cczh: sllz + lgvd
        |zczc: 2
        |ptdq: humn - dvpt
        |dvpt: 3
        |lfqf: 4
        |humn: 5
        |ljgn: 2
        |sjmn: drzm * dbpl
        |sllz: 4
        |pppw: cczh / lfqf
        |lgvd: ljgn * ptdq
        |drzm: hmdt - zczc
        |hmdt: 32""".stripMargin.linesIterator
    star1Task(t) shouldBe 152
    answerOf(star1Task) shouldBe 232974643455000L
    star2Task(t) shouldBe 301
    answerOf(star2Task) shouldBe 3740214169961L
