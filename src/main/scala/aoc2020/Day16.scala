package aoc2020

import scala.annotation.tailrec

case object Day16 extends Day:
  type Ticket = Vector[Long]
  case class Field(name: String, validRanges: Seq[(Long, Long)]):
    def isValid(value: Long): Boolean = validRanges.exists {
      case (min, max) => min <= value && value <= max
    }

  case class Input(fields: Seq[Field], ticket: Ticket, tickets: Seq[Ticket]):
    def findPossibleFields(value: Long) = fields.filter(_.isValid(value))
    def isInvalid(ticket: Ticket): Boolean = ticket.exists(v => findPossibleFields(v).isEmpty)
    def errorRate: Long = tickets.iterator.flatten.filter(v => findPossibleFields(v).isEmpty).sum
    def detectFieldsOrder: Seq[String] =
      val valid = (tickets :+ ticket).filterNot(isInvalid)
      val options = valid.foldLeft(ticket.map(_ => fields)) { (fields, ticket) =>
        for (fs, value) <- fields zip ticket yield fs.filter(_.isValid(value))
      }
      @tailrec def collect(opts: Vector[Set[String]]): Vector[String] =
        val defined = opts.map {
          case single if single.size == 1 => single.headOption
          case empty if empty.isEmpty => sys.error(s"No options left: $opts")
          case _ => None
        }
        if defined.forall(_.isDefined) then defined.map(_.get)
        else collect {
          val definedSet = defined.flatten.toSet
          for options <- opts
          yield
            if options.size == 1 then options
            else options -- definedSet
        }

      collect(options.map(_.map(_.name).toSet))
  
  private val FieldPattern = """([^:]+): (\d+)-(\d+) or (\d+)-(\d+)""".r
  
  def parse(lines: IterableOnce[String]): Input =
    val it = lines.iterator
    val fields = it.takeWhile(_.nonEmpty).toVector
    it.next() shouldBe "your ticket:"
    val ticket = it.next()
    it.next() shouldBe ""
    it.next() shouldBe "nearby tickets:"
    Input(
      fields.map {
        case FieldPattern(name, a, b, c, d) => Field(name, Seq(a.toLong -> b.toLong, c.toLong -> d.toLong))
      },
      ticket.split(",").toVector.map(_.toLong),
      it.map(_.split(",").toVector.map(_.toLong)).toVector
    )
  
  override def test(): Unit =
    val sample = parse(
      """
        |class: 1-3 or 5-7
        |row: 6-11 or 33-44
        |seat: 13-40 or 45-50
        |
        |your ticket:
        |7,1,14
        |
        |nearby tickets:
        |7,3,47
        |40,4,50
        |55,2,20
        |38,6,12
        |""".stripMargin.trim.linesIterator.toVector)
    sample.errorRate shouldBe 71L
    sample.detectFieldsOrder shouldBe Seq("row", "class", "seat")

  override def star1(): Any = readInput(parse).errorRate

  override def star2(): Any =
    val input = readInput(parse)
    val fields = input.detectFieldsOrder
    val departures = for 
      (field, value) <- fields zip input.ticket if field startsWith "departure" 
    yield value
    departures.product
