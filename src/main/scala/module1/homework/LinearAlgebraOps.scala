package module1.homework

object LinearAlgebraOps{
  def sum(v1: Array[Int], v2: Array[Int]): Array[Int] = {
    if (v1.length != v2.length)
      throw new Exception("Operation is not supported") {
      } else {
      for (i <- v1.indices) {
        v1(i) = v1(i) + v2(i)
      }
      v1
    }
  }

  def scale(a: Int, v: Array[Int]): Array[Int] = {
    v.map(x => a * x)
  }

  def ahru(a: Int, v1: Array[Int], v2: Array[Int]): Array[Int] = {
    sum(scale(a,v1),v2)
  }
}