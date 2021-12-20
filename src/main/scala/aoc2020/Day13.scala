package aoc2020

import common.Day

case object Day13 extends Day:
  override def test(): Unit =
    val sample =
      """
        |939
        |7,13,x,x,59,x,31,19
        |""".stripMargin.trim.linesIterator.toVector
    parse(sample).next shouldBe (5, 59)
    parse(sample).goldTime shouldBe 1068781
    // 1594873010624 + 2081330972747 (37+66) -> 59872140247540
    // 1594873010690 + 2081330972747 % 37
    // 22 + 23 % 37 = 3811 ~ 
//    println((59872140247540L - 1594873010624L) / 2081330972747L)
//    println(Iterator.from(22, 23).find(_ % 37 == 0))
//    println((666 - 22) / 23)
//    sys.exit(1)

  def gcd(a: Long, b: Long): Long =
    if a < b then gcd(b, a)
    else if b == 0 then a
    else gcd(b, a % b)
  
  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a,b)
  
  case class World(time: Int, buses: Seq[Option[Int]]):
    def next = buses.iterator
      .collect { 
        case Some(n) =>
          val next = math.ceil(time.toDouble / n).toInt * n
          (next - time, n)
      }
      .minBy(_._1)
  
    def goldTime =
      case class Bus(id: Int, offset: Int):
        override def toString: String = s"#$id+$offset"
        def arrivesAt(t: Long) = (t + offset) % id == 0

      def findMatch(bus: Bus, offset: Long, step: Long) =
        Iterator.iterate(offset)(_ + step).find(bus.arrivesAt).get

      val buses = this.buses.zipWithIndex.collect {
        case (Some(n), i) => Bus(n, i)
      }

      val (res, _) = buses.foldLeft((0L, 1L)) {
        case ((start, step), bus) =>
//          print(s"$bus".padTo(10, '.'))
          val arrive = findMatch(bus, start, step)
          val next = lcm(step, bus.id.toLong)
//          print(s" arrives at $arrive, next step is $next ".padTo(70, ' '))
//          println(buses.map(b => if b.arrivesAt(arrive) then '#' else '.').mkString("[","","]"))
          (arrive, next)
      }
      res
  
  val input =
    """
      |1002561
      |17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,409,x,29,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,373,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19
      |""".stripMargin.trim.linesIterator.toVector
  
  def parse(lines: IterableOnce[String]) =
    val it = lines.iterator
    World(it.next().toInt, it.next().split(",").toVector.map {
      case "x" => None
      case  x  => Some(x.toInt)
    })
  
  override def star1(): Any = 
    val (time, bus) = parse(input).next
    println(s"Wait $time for bus $bus")
    time * bus

  override def star2(): Any = parse(input).goldTime