package aoc2020

import common.Day

import scala.annotation.tailrec
import common.coord.*

case object Day24 extends Day:
  override def test(): Unit =
    move(parse("nwwswee")) shouldBe zero[Int2]
  
    val sample =
      """
        |sesenwnenenewseeswwswswwnenewsewsw
        |neeenesenwnwwswnenewnwwsewnenwseswesw
        |seswneswswsenwwnwse
        |nwnwneseeswswnenewneswwnewseswneseene
        |swweswneswnenwsewnwneneseenw
        |eesenwseswswnenwswnwnwsewwnwsene
        |sewnenenenesenwsewnenwwwse
        |wenwwweseeeweswwwnwwe
        |wsweesenenewnwwnwsenewsenwwsesesenwne
        |neeswseenwwswnwswswnw
        |nenwswwsewswnenenewsenwsenwnesesenew
        |enewnwewneswsewnwswenweswnenwsenwsw
        |sweneswneswneneenwnewenewwneswswnese
        |swwesenesewenwneswnwwneseswwne
        |enesenwswwswneneswsenwnewswseenwsese
        |wnwnesenesenenwwnenwsewesewsesesew
        |nenewswnwewswnenesenwnesewesw
        |eneswnwswnwsenenwnwnwwseeswneewsenese
        |neswnwewnwnwseenwseesewsenwsweewe
        |wseweeenwnesenwwwswnew
        |""".stripMargin.trim.linesIterator
    val tiles = Tiles().flip(parse(sample).map(move))
    tiles.black.size shouldBe 10
  
    tiles.after(100).black.size shouldBe 2208

  enum HexDir:
    case E, SE, SW, W, NW, NE
  
  import HexDir._
  
  def parse(line: String) =
    @tailrec def collect(ds: List[Char], res: List[HexDir]): List[HexDir] =
      ds match
        case 'e' ::        rest => collect(rest, E  :: res)
        case 's' :: 'e' :: rest => collect(rest, SE :: res)
        case 's' :: 'w' :: rest => collect(rest, SW :: res)
        case 'w' ::        rest => collect(rest, W  :: res)
        case 'n' :: 'w' :: rest => collect(rest, NW :: res)
        case 'n' :: 'e' :: rest => collect(rest, NE :: res)
        case Nil => res.reverse
        case other => sys.error(s"Unexpected line: ${other.mkString}")
    collect(line.toList, Nil)
  
  def parse(ls: IterableOnce[String]): Iterator[List[HexDir]] = ls.iterator.map(parse)

  //   NW   NE
  // W -- * -- E
  //   SW   SE
  val directions = Map(
    E  -> Int2( 2, 0),
    SE -> Int2( 1,-1),
    SW -> Int2(-1,-1),
    W  -> Int2(-2, 0),
    NW -> Int2(-1, 1),
    NE -> Int2( 1, 1)
  )
  def adjacentTo(pos: Int2) = HexDir.values.iterator.map(d => directions(d) + pos)
  
  def move(ds: List[HexDir]) = ds.iterator.map(directions).foldLeft(zero[Int2])(_ + _)
  
  def count[T](it: IterableOnce[T]): Map[T, Int] = it.iterator
    .foldLeft(Map.empty[T, Int] withDefaultValue 0) { (acc, t) =>
      acc.updated(t, acc(t) + 1)
    }
  
  case class Tiles(black: Set[Int2] = Set.empty):
    def flip(ts: IterableOnce[Int2]) = copy(ts.iterator.foldLeft(black) { (black, t) =>
      if black(t) then black - t
      else black + t
    })
    def flipByRules =
      val off = black.filter { b =>
        val blacks = adjacentTo(b).count(black)
        blacks == 0 || blacks > 2
      }
      val white = black.flatMap(adjacentTo).filterNot(black)
      val on = white.filter { w =>
        val blacks = adjacentTo(w).count(black)
        blacks == 2
      }
      copy(black -- off ++ on)
    def after(flips: Int) = Iterator.iterate(this)(_.flipByRules).drop(flips).next()
  
  override def star1(): Any = readInput(ls => count(parse(ls).map(move))).valuesIterator.count(_ % 2 == 1)

  override def star2(): Any = readInput(ls => Tiles().flip(parse(ls).map(move))).after(100).black.size