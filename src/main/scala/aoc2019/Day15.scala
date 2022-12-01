package aoc2019

import common.*
import common.coord.*

case object Day15 extends Day:
  case class Map2D(tiles: Map[Int2, String] = Map.empty):
    def updated(pos: Int2, tile: String) = copy(tiles.updated(pos, tile))
    def render(path: Edge[Int2]*): Unit =
      render2d(tiles.keySet, { p =>
        if path.exists(e => e.start == p || e.end == e) then "•"
        else tiles(p)
      })

  object Map2D:
    given Graph[Map2D] with
      type Node = Int2
      extension (g: Map2D) def edgesFrom(a: Int2) = a.neighbours.map(Edge(a, _))

    def parse(ls: IterableOnce[String]) =
      val tiles = for (l, y) <- ls.iterator.zipWithIndex; (t, x) <- l.split(".").zipWithIndex yield Int2(x, y) -> t
      Map2D(tiles.toMap)
