package aoc2019

import common.*

import scala.annotation.tailrec

case object Day14 extends Day:
  case class Measure(chemical: String, quantity: Long = 1):
    override def toString: String = s"$quantity $chemical"
    def * (r: Long) = copy(quantity = quantity * r)
    def + (m: Measure) =
      require(chemical == m.chemical)
      copy(quantity = quantity + m.quantity)

  type Recipies = Map[String, (Measure, Seq[Measure])]

  def parse(reactions: Iterator[String]): Recipies =
    def asMeasure(str: String) = str.split(" ") match
      case Array(q, c) => Measure(c, q.toLong)
    reactions
      .map { reaction =>
        val Array(ingredients, result) = reaction.split(" => ")
        val m = asMeasure(result)
        val ms = ingredients.split(", ").view.map(asMeasure).toVector
        m.chemical -> (m, ms)
      }
      .toMap

  extension(recipies: Recipies)
    def allIngredientsOf(c: Iterable[String]): Set[String] = c.toSet.flatMap(allIngredientsOf)
    def allIngredientsOf(c: String): Set[String] =
      recipies.get(c) match
        case None => Set.empty
        case Some((_, ms)) =>
          val current = ms.view.map(_.chemical).toSet
          current ++ allIngredientsOf(current)

    def resolve(chemical: String, quantity: Long = 1) =
      @tailrec def recur(needed: Seq[Measure]): Seq[Measure] =
        val furtherIngredient = recipies.allIngredientsOf(needed.map(_.chemical))
        val updated = needed
          .flatMap { needed =>
            recipies.get(needed.chemical) match {
              case Some((rate, ingridients)) if !furtherIngredient(needed.chemical) =>
                val minQuantity = needed.quantity / rate.quantity
                val quantity = if needed.quantity % rate.quantity == 0 then minQuantity else minQuantity + 1
                ingridients.map(_ * quantity)

              case _ => Seq(needed)
            }
          }
          .groupMapReduce(_.chemical)(identity)(_ + _)
          .valuesIterator.toVector
        if updated == needed then needed
        else recur(updated)

      recur(Seq(Measure(chemical, quantity)))

    def resolveRate(chenical: String) =
      @tailrec def recur(needed: Map[String, Double]): Map[String, Double] =
        if !needed.keys.exists(recipies.contains) then needed
        else recur(
          needed.toVector
            .flatMap { case n @ (c, q) =>
              recipies.get(c) match
                case None => Seq(n)
                case Some((rate, ingrs)) => ingrs.map(m => m.chemical -> (m.quantity * q / rate.quantity))
            }
            .groupMapReduce(_._1)(_._2)(_ + _)
        )
      recur(Map(chenical -> 1.0))

    def resolveHaving(chemical: String, source: Measure): Long =
      val needFor1 = resolve(chemical).find(_.chemical == source.chemical).get.quantity
      val averageFor1 = resolveRate(chemical)(source.chemical)
      val lowerBound = source.quantity / needFor1
      val upperBound = math.ceil(source.quantity / averageFor1).toLong
      Iterator.iterate(upperBound)(_ - 1)
        .map(q => q -> resolve(chemical, q).find(_.chemical == source.chemical).get.quantity)
        .collectFirst { case (out, inp) if inp <= source.quantity => out }
        .get

  val Trillion = 1_000_000_000_000L

  override def test(): Unit =
    val t1 = parse(
      """
        |9 ORE => 2 A
        |8 ORE => 3 B
        |7 ORE => 5 C
        |3 A, 4 B => 1 AB
        |5 B, 7 C => 1 BC
        |4 C, 1 A => 1 CA
        |2 AB, 3 BC, 4 CA => 1 FUEL
        |""".stripMargin.trim.linesIterator)
    t1.resolve("FUEL") shouldBe Vector(Measure("ORE", 165))
    val t2 = parse(
      """
        |157 ORE => 5 NZVS
        |165 ORE => 6 DCFZ
        |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
        |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
        |179 ORE => 7 PSHF
        |177 ORE => 5 HKGWZ
        |7 DCFZ, 7 PSHF => 2 XJWVT
        |165 ORE => 2 GPVTF
        |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
        |""".stripMargin.trim.linesIterator)
    t2.resolve("FUEL") shouldBe Vector(Measure("ORE", 13_312))
    t2.resolveHaving("FUEL", Measure("ORE", Trillion)) shouldBe 82_892_753
    val t3 = parse(
      """
        |2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
        |17 NVRVD, 3 JNWZP => 8 VPVL
        |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
        |22 VJHF, 37 MNCFX => 5 FWMGM
        |139 ORE => 4 NVRVD
        |144 ORE => 7 JNWZP
        |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
        |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
        |145 ORE => 6 MNCFX
        |1 NVRVD => 8 CXFTF
        |1 VJHF, 6 MNCFX => 4 RFSQX
        |176 ORE => 6 VJHF
        |""".stripMargin.trim.linesIterator)
    t3.resolve("FUEL") shouldBe Vector(Measure("ORE", 180_697))
    t3.resolveHaving("FUEL", Measure("ORE", Trillion)) shouldBe 5_586_022
    val t4 = parse(
      """
        |171 ORE => 8 CNZTR
        |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
        |114 ORE => 4 BHXH
        |14 VRPVC => 6 BMBT
        |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
        |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
        |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
        |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
        |5 BMBT => 4 WPTQ
        |189 ORE => 9 KTJDG
        |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
        |12 VRPVC, 27 CNZTR => 2 XDBXC
        |15 KTJDG, 12 BHXH => 5 XCVML
        |3 BHXH, 2 VRPVC => 7 MZWV
        |121 ORE => 7 VRPVC
        |7 XCVML => 6 RJRHP
        |5 BHXH, 4 VRPVC => 5 LTCX
        |""".stripMargin.trim.linesIterator)
    t4.resolve("FUEL") shouldBe Vector(Measure("ORE", 2210736))
    t4.resolveHaving("FUEL", Measure("ORE", Trillion)) shouldBe 460_664

  override def star1(): Any =
    val Seq(Measure("ORE", q)) = readInput(parse).resolve("FUEL")
    q shouldBe 741_927

  override def star2(): Any =
    readInput(parse).resolveHaving("FUEL", Measure("ORE", Trillion)) shouldBe 2_371_699

