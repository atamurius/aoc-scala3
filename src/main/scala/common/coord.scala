package common

import scala.annotation.tailrec

object coord:

  trait Vec[C]:
    type Item

    def build(xs: IterableOnce[Item]): C

    val size: Int

    def axes: Range = 0 until size

    def zip(a: C, b: C)(f: (Item, Item) => Item): C =
      build(for (a, b) <- components(a) zip components(b) yield f(a, b))

    extension (a: C)
      def components: Iterator[Item]
      def component(i: Int): Item = a.components.drop(i).next()
      def show: String = a.components.mkString("(", "; ", ")")
      def map(f: Item => Item): C = build(a.components map f)
      def mapWithIndex(f: (Item, Int) => Item): C = build(a.components.zipWithIndex map f.tupled)
      def update(i: Int, item: Item): C = mapAt(i, _ => item)
      def mapAt(i: Int, f: Item => Item): C =
        build(components.zipWithIndex.map {
          case (c, `i`) => f(c)
          case (c,   _) => c
        })

    extension (a: C)(using N: Numeric[Item])
      def +(b: C): C = zip(a, b)(N.plus)
      def *(b: C): C = zip(a, b)(N.times)
      def *(f: Item)(using DummyImplicit): C = a.map(N.times(_, f))
      def unary_- : C = a.map(N.negate)
      def -(b: C): C = a + -b
      def norm: Item = a.components.map(N.abs).sum
      def adjascent: Iterator[C] = adjascentAndSelf.filter(_ != a)
      def adjascentAndSelf: Iterator[C] = cubeAt(a)(using this)
      def neighbours: Iterator[C] =
        for d <- Iterator(N.one, N.negate(N.one)); i <- axes yield a.mapAt(i, N.plus(_, d))
      def multiple: Item = a.components.product

    extension (a: C)(using N: Integral[Item])
      def /(f: Item): C = a.map(N.quot(_, f))
      def %(f: Item): C = a.map(N.rem(_, f))
      def |%|(b: C): C = zip(a, b)(N.rem)
      def unite: C = a / a.components.filter(_ != N.zero).map(N.abs).min

    extension (a: C)(using O: Ordering[Item])
      def in(cube: (C, C)): Boolean =
        import O._
        (cube._1.components zip a.components zip cube._2.components).forall {
          case ((l, m), r) => l <= m && m <= r
        }

  object V:
    def apply[C](using C: Vec[C]): C.type = C

  def zero[C](using V: Vec[C])(using N: Numeric[V.Item]): C = V.build(Iterator.continually(N.zero).take(V.size))

  def cubeAt[C](center: C)(using V: Vec[C])(using N: Numeric[V.Item]): Iterator[C] =
    val values = Seq(N.one, N.zero, N.negate(N.one))
    center
      .components
      .foldLeft(Iterator(List.empty[V.Item])) { (it, c) =>
        for
          tail <- it
          d <- values
        yield N.plus(d, c) :: tail
      }
      .map(_.reverse)
      .map(V.build)

  def boundingBox[C](xs: Iterable[C])(using V: Vec[C])(using N: Ordering[V.Item]): (C, C) =
    xs.foldLeft(xs.head, xs.head) {
      case ((min, max), x) => (
        V.zip(min, x)(N.min),
        V.zip(max, x)(N.max)
      )
    }

  /** @returns minimal vector S that A + K*S = B */
  def discreteStepTowards[C](a: C, b: C)(using V: Vec[C])(using N: Integral[V.Item]): C =
    import N.*
    val deltas = V.zip(b, a)(_ - _)
    if deltas.components.forall(_ == N.zero) then deltas
    else
      @tailrec def gcd(a: V.Item, b: V.Item): V.Item =
        if a < b then gcd(b, a)
        else if b == N.zero then a
        else gcd(b, a % b)
      val divisor = deltas.components.map(abs).reduce(gcd)
      deltas.map(_ / divisor)

  def discreteLine[C](a: C, b: C)(using V: Vec[C])(using N: Integral[V.Item]): Iterator[C] =
    val step = discreteStepTowards(a, b)
    Iterator.iterate(a)(_ + step).takeWhile(_ != b) ++ Iterator(b)

  given Vec[Int] with
    type Item = Int

    extension (v: Int) def components: Iterator[Int] = Iterator(v)

    def build(xs: IterableOnce[Item]): Int = xs.iterator.next()

    val size: Int = 1

  case class Int2(x: Int, y: Int):
    override def toString: String = this.show

    def rotateTimes(times: Int) =
      val left = if times < 0 then math.ceil(-times / 4.0).toInt * 4 + times else times
      (1 to left).foldLeft(this) {
        case (Int2(x, y), _) => Int2(-y, x)
      }
  // (2, 1) - left => (-1, 2)

  private def simpleRender: Int2 => String = _ => Color.bright("#")

  def render2d(ps: Set[Int2], render: Int2 => String = simpleRender, width: Int = 1): Unit =
    val (tl, br) = boundingBox(ps)
    println()
    for y <- tl.y to br.y do
      val line = for x <- tl.x to br.x yield
        val p = Int2(x,y)
        if ps(p) then render(p) else Color.blue(".".padTo(width, ' '))
      println(line.mkString(" "))
    println()

  given Vec[Int2] with
    type Item = Int
    extension (v: Int2) def components: Iterator[Int] = Iterator(v.x, v.y)

    def build(xs: IterableOnce[Item]): Int2 =
      val it = xs.iterator
      Int2(it.next(), it.next())

    val size: Int = 2

  enum Dir(val delta: Int2):
    case N extends Dir(Int2(0, 1))
    case E extends Dir(Int2(1, 0))
    case S extends Dir(Int2(0, -1))
    case W extends Dir(Int2(-1, 0))

    def left = this match
      case N => W
      case E => N
      case S => E
      case W => S

    def right = this match
      case N => E
      case E => S
      case S => W
      case W => N

    def turn(left: Boolean, times: Int): Dir =
      if left then (1 to times).foldLeft(this)((d, _) => d.left)
      else (1 to times).foldLeft(this)((d, _) => d.right)

    def turn(degrees: Int): Dir =
      turn(left = true, (360 + degrees) % 360 / 90)

  case class Int3(x: Int, y: Int, z: Int):
    override def toString: String = this.show

  given Vec[Int3] with {
    type Item = Int
    extension (v: Int3) def components: Iterator[Int] = Iterator(v.x, v.y, v.z)

    def build(xs: IterableOnce[Int]): Int3 =
      val it = xs.iterator
      Int3(it.next(), it.next(), it.next())

    val size: Int = 3
  }

  case class Int4(x: Int, y: Int, z: Int, w: Int):
    override def toString: String = this.show

  given Vec[Int4] with {
    type Item = Int
    extension (v: Int4) def components: Iterator[Int] = Iterator(v.x, v.y, v.z, v.w)

    def build(xs: IterableOnce[Int]): Int4 =
      val it = xs.iterator
      Int4(it.next(), it.next(), it.next(), it.next())

    val size: Int = 4
  }
