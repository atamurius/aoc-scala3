package common

import java.util.concurrent.Future
import scala.collection.immutable.::

object Test extends App {

  case class Box(value: Any)(implicit val ordering: Ordering[_ >: value.type])

  val t = Box(42)

  println(t.ordering.compare(t.value, t.value))
}
