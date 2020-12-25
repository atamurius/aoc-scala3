package aoc2020

case object Day21 extends Day:
  override def test(): Unit =
    val sample = parse(
      """
        |mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
        |trh fvjkl sbzzf mxmxvkd (contains dairy)
        |sqjhc fvjkl (contains soy)
        |sqjhc mxmxvkd sbzzf (contains fish)
        |""".trim.stripMargin.linesIterator)
    sample.count(sample.noAlergens) shouldBe 5
    sample.evalAlergens.get shouldBe Map("dairy" -> "mxmxvkd", "fish" -> "sqjhc", "soy" -> "fvjkl")

  private val FoodFormat = """([^(]+) \(contains ([^)]+)\)""".r
  
  def parse(lines: IterableOnce[String]) = Food(
    lines.iterator.map {
      case FoodFormat(ings, alergens) => (ings.split(" ").toSet, alergens.split(", ").toSet)
    }.toVector)
  
  class Food(componentsAndAlergens: Seq[(Set[String], Set[String])]):
    val components: Set[String] = componentsAndAlergens.view.flatMap(_._1).toSet
    val alergens: Set[String] = componentsAndAlergens.view.flatMap(_._2).toSet
    val possibleComponents = componentsAndAlergens.foldLeft(Map.empty[String, Set[String]]) {
      case (map, (components, alergens)) => 
        alergens.foldLeft(map) { (map, alergen) =>
          map.updated(alergen, map.get(alergen).fold(components)(_ intersect components))
        }
    }
    val noAlergens = components -- possibleComponents.values.flatten

    def count(components: Iterable[String]) = componentsAndAlergens
      .map((cs, _) => components.count(cs))
      .sum

    def evalAlergens =
      def recurse(current: Map[String, Set[String]]): Option[Map[String, String]] =
        if current.exists(_._2.isEmpty) then None
        else
          val nonDetermined = current.iterator.flatMap {
            case (alergen, comps) if comps.size > 1 => comps.map(alergen -> _)
            case _ => Nil
          }
          if !nonDetermined.hasNext then Some(current.collect { case (a, cs) => a -> cs.head })
          else nonDetermined
            .flatMap { (a, c) =>
              recurse(current.transform((_, cs) => cs - c) + (a -> Set(c)))
            }
            .toSeq.headOption
      recurse(possibleComponents)

  end Food
  
  override def star1(): Any =
    val food = readInput(parse)
    food.count(food.noAlergens)

  override def star2(): Any =
    val food = readInput(parse)
    val alergens = food.evalAlergens.get
    alergens.toVector.sortBy(_._1).map(_._2).mkString(",")