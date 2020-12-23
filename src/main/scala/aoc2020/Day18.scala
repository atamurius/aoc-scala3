package aoc2020

import scala.annotation.tailrec

case object Day18 extends Day:
  
  val rules1 = Map(
    '+' -> Operation(_ + _, 1),
    '*' -> Operation(_ * _, 1)
  )
  
  val rules2 = Map(
    '+' -> Operation(_ + _, 2),
    '*' -> Operation(_ * _, 1)
  )
  
  override def star1(): Any = readInput(_.map(evaluate(_, rules1)).sum)
  
  override def star2(): Any = readInput(_.map(evaluate(_, rules2)).sum)

  override def test(): Unit =
    evaluate("1 + 2 * 3 + 4 * 5 + 6", rules1) shouldBe 71
    evaluate("1 + (2 * 3) + (4 * (5 + 6))", rules1) shouldBe 51
    evaluate("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", rules1) shouldBe 12240
    evaluate("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", rules1) shouldBe 13632
    evaluate("2 + ((3 * 4) * 5)", rules1) shouldBe 62

    evaluate("1 + (2 * 3) + (4 * (5 + 6))", rules2) shouldBe 51
    evaluate("2 * 3 + (4 * 5)", rules2) shouldBe 46
    evaluate("5 + (8 * 3 + 9 + 3 * 4 * 3)", rules2) shouldBe 1445
    evaluate("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", rules2) shouldBe 669060
    evaluate("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", rules2) shouldBe 23340

  case class Operation(run: (Long, Long) => Long, priority: Int)
  
  def evaluate(line: String, operations: Map[Char, Operation], debug: Boolean = false) =
    import Character._
    
    sealed trait State
    case object Empty extends State
    case class Value(value: Long) extends State:
      override def toString: String = value.toString
    case class Pending(prev: Option[Pending], left: Long, op: Char) extends State:
      override def toString: String = s"${prev.fold("")(_.toString)}$left $op "
    case class Lazy(pending: Pending, right: Long) extends State:
      override def toString: String = s"$pending$right"
       
    def run(x: Long, op: Char, y: Long) = operations.getOrElse(op, sys.error(s"Undefined operation: $op")).run(x, y)
    
    @tailrec def strict(l: Lazy): Long =
      val value = run(l.pending.left, l.pending.op, l.right)
      l.pending.prev match
        case None => value
        case Some(p) => strict(new Lazy(p, value))
    
    val initial = line.filterNot(isWhitespace)
    
    @tailrec def eval(
      next: List[Char],
      current: State,
      suspended: List[Pending|Empty.type]
    ): Long = 
      def position = s"${initial.dropRight(next.size)}|${next.mkString}"
      def requireState[T](f: PartialFunction[State, T]): T = 
        f.applyOrElse(current, c => sys.error(s"Unexpected state at $position: $c"))
      def forceValue = requireState {
        case Value(result) => result
        case l: Lazy => strict(l)
      }
      if debug then println(s"$position: $current (${suspended mkString " :: "})")
      next match
        case Nil => 
          require(suspended.isEmpty)
          forceValue
  
        case n :: rest if isDigit(n) =>
          val value = n.toString.toLong 
          eval(
            next = rest,
            suspended = suspended,
            current = requireState {
              case Empty => Value(value)
              case p: Pending => Lazy(p, value)
            }
          )
          
        case '(' :: rest => eval(
          next = rest, 
          current = Empty,
          requireState {
            case Empty => Empty :: suspended
            case p: Pending => p :: suspended
          }
        )

        case ')' :: rest => suspended match
          case (pending: Pending) :: restSuspended => 
            eval(rest, Lazy(pending, forceValue), restSuspended)
          case Empty :: restSuspended => 
            eval(rest, Value(forceValue), restSuspended)
          case Nil =>
            eval(rest, Value(forceValue), Nil)

        case op :: rest => 
          require(operations.contains(op), s"Unknown operation: $op at $position")
          def priority(op: Char) = operations(op).priority
          // ensure priority(prev.op) < priority(op)
          @tailrec def fold(prev: Option[Pending], right: Long, op: Char): State = prev match
            case None => Pending(None, right, op)
            case Some(p) if priority(p.op) < priority(op) => Pending(prev, right, op)
            case Some(Pending(prev, left, lop)) => fold(prev, run(left, lop, right), op)
          
          requireState {
            case Value(left) => eval(rest, Pending(None, left, op), suspended)
            case Lazy(p, right) => eval(rest, fold(Some(p), right, op), suspended)
          }

    eval(initial.toList, Empty, Nil)
