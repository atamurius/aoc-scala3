package aoc2022

import common.TypedDay

case object Day20 extends TypedDay:

  override def star1Task: Task = lines =>
    val values = lines.map(_.toLong).toVector
    val res = decrypt(values, values.indices.toVector)
    val zero = res.indexOf(values.indexOf(0))
    def after(n: Int) = values(res((zero + n) % values.size))
    val a = after(1000)
    val b = after(2000)
    val c = after(3000)
    a + b + c

  override def star2Task: Task = lines =>
    val values = lines.map(_.toLong * 811589153L).toVector
    val res = (1 to 10).foldLeft(values.indices.toVector) { (vs, _) =>
      decrypt(values, vs)
    }
    val zero = res.indexOf(values.indexOf(0))
    def after(n: Int) = values(res((zero + n) % values.size))
    val a = after(1000)
    val b = after(2000)
    val c = after(3000)
    a + b + c

  def decrypt(values: Vector[Long], indices: Vector[Int]): Vector[Int] =
    val size = values.size - 1
    def mod(i: Long) = ((i % size).toInt + size) % size
    values.indices.foldLeft(indices) { (indices, x) =>
      val a = indices.indexOf(x)
      val b = mod(a + values(x))
      if a < b then
        indices.take(a) ++ indices.slice(a + 1, b + 1) ++ Vector(x) ++ indices.drop(b + 1)
      else if b < a then
        indices.take(b) ++ Vector(x) ++ indices.slice(b, a) ++ indices.drop(a + 1)
      else indices
    }

  override def test(): Unit =
    def t =
      """
        |1
        |2
        |-3
        |3
        |-2
        |0
        |4
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 3
    answerOf(star1Task) shouldBe 23321L
    star2Task(t) shouldBe 1623178306L
