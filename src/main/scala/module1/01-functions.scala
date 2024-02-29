package module1

object functions {


  /**
   * Функции
   */



  /**
   * Реализовать ф-цию  sum, которая будет суммировать 2 целых числа и выдавать результат
   */

   def sum(x: Int, y: Int): Int = x + y

   sum(1, 2) // 3

   val sum2: (Int, Int) => Int = (v1: Int, v2: Int) => v1 + v2

   val sum3: (Int, Int) => Int = sum

   sum2(1, 2) // 3
   sum3(3, 4) // 7


  // Partial function

  val divide: PartialFunction[(Int, Int), Int] = {
    case x if x._2 != 0 => x._1 / x._2
  }

  if(divide.isDefinedAt(1, 0)){
    divide(1, 0)
  }

  val ll = List((4, 2), (5, 0), (6, 2)).collect(divide)


  // SAM Single Abstract Method

  trait Printer{
    def print(str: String): Unit
  }

  val p: Printer = str => println(str)



  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */


  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */



  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */


  /**
   * return statement
   *
   */
}