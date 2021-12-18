package aoc2021

import common._
import common.coord._

case object Day17 extends Day:
  type Speed = Int

  def shootX(speedX: Int): Iterator[(Int, Speed)] =
    def drag: Int => Int =
      case 0 => 0
      case x if x > 0 => x - 1
      case x => x + 1
    Iterator.iterate((0, speedX)) { (x, dx) => (x + dx, drag(dx)) }

  def shootY(speedY: Int): Iterator[(Int, Speed)] =
    Iterator.iterate((0, speedY)) { (y, dy) => (y + dy, dy - 1) }

  def shoot(speed: Int2): Iterator[Int2] = (shootX(speed.x) zip shootY(speed.y)).map((x,y) => Int2(x._1, y._1))

  type Area = (Int2, Int2)

  def hitStep(speed: Int2, target: Area): Option[Int] =
    shoot(speed).zipWithIndex.collectFirst {
      case (p, i) if p in target => Some(i)
      case (p, _) if p.y < target._1.y => None
    }.flatten

  def findMinHitX(target: Area): Option[Speed] =
    val targetXs = target._1.x to target._2.x
    def stoppedInTargetAt(startX: Int): Option[Int] =
      shootX(startX).zipWithIndex.collectFirst {
        case ((x, 0), i) if targetXs.contains(x) => Some(i)
        case ((_, 0), _) => None
      }.flatten
    (0 to target._2.x).map(dx => stoppedInTargetAt(dx) -> dx).find(_._1.isDefined).map(_._2)

  def drawShoot(speed: Int2, target: Area): Unit =
    val step = hitStep(speed, target)
    val t = shoot(speed).take(step.fold(1000)(_ + 1)).toSet
    val (tl, br) = boundingBox(t + target._1 + target._2)
    println(s"-> $speed, hit at ${step.fold("NEVER")(_.toString)}")
    for y <- br.y to target._1.y - 1 by -1 do
      var hit = false
      val line = for x <- tl.x to target._2.x + 1 yield
        val p = Int2(x, y)
        hit |= p in target
        if t(p) then Color.bright('#')
        else if p in target then Color.blue('T')
        else '.'
      println(line.mkString(" "))

  def maxHitY(target: Area) = -target._1.y - 1

  def minHitY(target: Area) = target._1.y

  def highestShoot(target: Area): Option[Int2] =
    findMinHitX(target).map(x => Int2(x, maxHitY(target)))

  def maxHight(speed: Int2) = (speed.y * (speed.y + 1))/2

  def allSpeeds(target: Area) =
    for
      dy <- minHitY(target) to maxHitY(target)
      minHitX = math.sqrt(target._1.x*2.0).toInt
      dx <- minHitX to target._2.x
      speed = Int2(dx, dy)
      if hitStep(speed, target).isDefined
    yield speed

  override def test(): Unit =
    val target = (Int2(20, -10), Int2(30, -5))
    val speed = highestShoot(target).get
    maxHight(speed) shouldBe 45
    val speeds = allSpeeds(target).toVector
    speeds.size shouldBe 112
//    drawShoot(speed, target)

  val inputTarget = (Int2(217, -126), Int2(240, -69))

  override def star1(): Any =
    val speed = highestShoot(inputTarget).get
//    drawShoot(speed, inputTarget)
    maxHight(speed)

  override def star2(): Any = allSpeeds(inputTarget).size