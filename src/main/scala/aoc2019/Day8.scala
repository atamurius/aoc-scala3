package aoc2019

import common.*

case object Day8 extends Day:
  type Layer = Vector[Vector[Int]]
  case class Image(layers: Seq[Layer]):
    def width: Int = layers.head.size
    def height: Int = layers.head.head.size
    def print(): Unit =
      for (layer, i) <- layers.zipWithIndex do
        println(s"layer: $i")
        for line <- layer do
          val colored = line.map {
            case 0 => Color.gray(" .")
            case 1 => Color.bright(" #")
            case x => Color.blue(f"$x%2d")
          }
          println(colored.mkString)
        println()

    def merge(transparent: Int): Image = Image(Seq(
      layers.reduce { (top, bottom) =>
        for (t, b) <- top zip bottom
          yield (t zip b).map {
            case (`transparent`, b) => b
            case (t, _)             => t
          }
      }
    ))

  def readImage(width: Int, height: Int, data: String): Image = Image {
    data.iterator.map(_.asDigit).grouped(width).map(_.toVector)
      .grouped(height).map(_.toVector).toVector
  }

  def inputImage: Image = readImage(25, 6, readInput(_.next()))

  override def test(): Unit =
    val l = readImage(3, 2, "123456789012").layers.minBy(_.flatten.count(_ == 0)).flatten
    l.count(_ == 1) * l.count(_ == 2) shouldBe 1
    readImage(2, 2, "0222112222120000").merge(2).layers.head shouldBe Vector(Vector(0, 1), Vector(1, 0))

  override def star1(): Any =
    val layer = inputImage.layers.minBy(_.flatten.count(_ == 0)).flatten
    layer.count(_ == 1) * layer.count(_ == 2) shouldBe 1792

  override def star2(): Any =
    val image = inputImage
    image.merge(2).print()

