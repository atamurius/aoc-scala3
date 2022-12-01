package common

import scala.annotation.tailrec
import common.*

case class Edge[N](start: N, end: N, weight: Int = 1)

trait Graph[G]:
  type Node
  extension (graph: G)
    def edgesFrom(a: Node): IterableOnce[Edge[Node]]

    def findPathMinSteps(start: Node, end: Node): Vector[Edge[Node]] =
      @tailrec def recur(front: Set[Node], edgeTo: Map[Node, Edge[Node]]): Vector[Edge[Node]] =
        edgeTo.get(end) match
          case Some(edge) =>
            unfoldIterator(edge)(edgeTo get _.end).toVector
          case None =>
            val next = front.flatMap(graph.edgesFrom).filterNot(edgeTo contains _.end)
            val nextFront = next.groupMapReduce(_.end)(identity)((e, _) => e)
            if nextFront.isEmpty then Vector.empty
            else recur(nextFront.keySet, edgeTo ++ nextFront)
      recur(Set(start), Map.empty)

    def findPathMinWeight(start: Node, end: Node): Vector[Edge[Node]] =
      @tailrec def recur(front: Set[Node], bestEdgeTo: Map[Node, Edge[Node]]): Vector[Edge[Node]] =
        if front.isEmpty then
          bestEdgeTo.get(end) match
            case None => Vector.empty
            case Some(edge) =>
              unfoldIterator(edge)(bestEdgeTo get _.start).takeUntil(_.start == start).toVector.reverse
        else
          val next = front.flatMap(graph.edgesFrom)
            .filter { edge =>
              bestEdgeTo.get(edge.end).forall(_.weight > edge.weight)
            }
            .groupMapReduce(_.end)(identity) { (a, b) => if a.weight > b.weight then b else a }
          recur(next.keySet, bestEdgeTo ++ next)
      recur(Set(start), Map.empty)
