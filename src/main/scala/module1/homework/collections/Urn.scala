package module1.homework.collections


import scala.util.Random

class Urn {

  private var bucket = List[Int](0, 0, 0, 1, 1, 1)

  def drawBalls(): Boolean = {
    val firstIndex = Random.nextInt(bucket.length)
    val first = bucket(firstIndex)
    bucket = bucket.patch(firstIndex, Nil, 1)

    if (first == 1 || bucket(Random.nextInt(bucket.length)) == 1) true else false
  }
}

object Urn {
  def apply(): Urn = new Urn()
}
