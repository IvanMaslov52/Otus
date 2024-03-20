import module1.homework.collections.Urn

object Main {

  def main(args: Array[String]): Unit = {
     println("Вероятность появление хотя бы одного шара равна 0.8")
     val start = System.currentTimeMillis()
     val bucket = Vector.fill(10000)(Urn())
     val result = bucket.map(_.drawBalls()).count({ result => result }) / 10000.0
     println(s"Вероятность появление хотябы одного шара в результате эксперимента равна: $result")
     val end = System.currentTimeMillis()
     println("Разница: " + (0.8 - result))
     println("Running time: " + (end - start))
  }
}