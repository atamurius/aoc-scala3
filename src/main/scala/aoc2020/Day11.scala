package aoc2020

case object Day11 extends Day:
  
  enum Seat(val display: Char):
    case Floor extends Seat('.')
    case Empty extends Seat('L')
    case Occupied extends Seat('#')

  object Seat:
    def parse(c: Char) = Seat.values.find(_.display == c).getOrElse(sys.error(s"Unknown state: $c"))
  
  trait SeatRules:
    def transition(map: SeatMap, x: Int, y: Int): Seat

    def next(map: SeatMap) = SeatMap(
      for (row, y) <- map.rows.zipWithIndex yield
        for (seat, x) <- row.zipWithIndex yield
        transition(map, x, y)
    )

    def stable(map: SeatMap) = Iterator.iterate(map)(next)
      .sliding(2)
      .collectFirst {
        case Seq(a, b) if a.rows == b.rows => a
      }
      .get

  object AreaSeatRules extends SeatRules:
    def area(map: SeatMap, x: Int, y: Int): Iterator[Seat] =
      for
        i <- (x - 1 to x + 1).iterator if i >= 0 && i < map.width
        j <- (y - 1 to y + 1).iterator if j >= 0 && j < map.height
        if i != x || j != y
      yield map(i, j)

    override def transition(map: SeatMap, x: Int, y: Int): Seat =
      map(x, y) match
        case Seat.Empty if area(map, x,y).forall(_ != Seat.Occupied) => Seat.Occupied
        case Seat.Occupied if area(map, x,y).count(_ == Seat.Occupied) >= 4 => Seat.Empty
        case other => other

  object ViewSeatRules extends SeatRules:
    def look(map: SeatMap, x: Int, y: Int, dx: Int, dy: Int): Option[Seat] =
      Iterator.iterate((x, y)) { case (x, y) => (x + dx, y + dy) }
        .drop(1)
        .map { case (x, y) => map.get(x, y) }
        .collectFirst {
          case x if x.forall(_ != Seat.Floor) => x
        }
        .get

    def lookForOccupied(map: SeatMap, x: Int, y: Int) =
      val dirs = for
        dx <- (-1 to 1).iterator
        dy <- (-1 to 1).iterator
        if dx != 0 || dy != 0
      yield look(map, x, y, dx, dy)
      dirs.count(_.contains(Seat.Occupied))
    
    override def transition(map: SeatMap, x: Int, y: Int): Seat =
      map(x, y) match
        case Seat.Empty if lookForOccupied(map, x, y) == 0 => Seat.Occupied
        case Seat.Occupied if lookForOccupied(map, x, y) >= 5 => Seat.Empty
        case other => other


  case class SeatMap(rows: Vector[Vector[Seat]]):
    def this(lines: Iterator[String]) = this(lines.map(_.toVector.map(Seat.parse)).toVector)
    def width = rows.head.size
    def height = rows.size
    def debug(): Unit = rows.foreach { row =>
      println(row.iterator.map(_.display).mkString)
    }
    def iterator = rows.iterator.flatMap(_.iterator)
    def count(s: Seat) = iterator.count(_ == s)
    def apply(x: Int, y: Int) = rows(y)(x)
    def get(x: Int, y: Int) = rows.lift(y).flatMap(_.lift(x)) 

  override def test(): Unit =
    val sample = new SeatMap(
      """
        |L.LL.LL.LL
        |LLLLLLL.LL
        |L.L.L..L..
        |LLLL.LL.LL
        |L.LL.LL.LL
        |L.LLLLL.LL
        |..L.L.....
        |LLLLLLLLLL
        |L.LLLLLL.L
        |L.LLLLL.LL
        |""".stripMargin.trim.linesIterator)
    AreaSeatRules.stable(sample).count(Seat.Occupied) shouldBe 37
    ViewSeatRules.stable(sample).count(Seat.Occupied) shouldBe 26
    

  override def star1(): Any = AreaSeatRules.stable(readInput(new SeatMap(_))).count(Seat.Occupied)
  
  override def star2(): Any = ViewSeatRules.stable(readInput(new SeatMap(_))).count(Seat.Occupied)
