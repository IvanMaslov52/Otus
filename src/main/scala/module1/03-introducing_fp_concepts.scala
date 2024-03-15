package module1

import module1.list.List
import module1.list.List.{::, Nil}
import module1.opt.{None, Some}

import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



// recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n *= i
      i += 1
    }
    _n
  }

  def factRec(n: Int): Int = if (n <= 0) 1 else n * factRec(n - 1)

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else loop(n - 1, n * acc)
    }

    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */
  def fibonacci(n: Int): Int = {
    @tailrec
    def loop(a: Int, b: Int, n: Int): Int = n match {
      case 0 => a
      case _ => loop(b, a + b, n - 1)
    }

    if (n <= 1) n
    else loop(0, 1, n)
  }


}

object hof {


  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
    result
  }

  def doomy(str: String): Unit = {
    Thread.sleep(1000)
    println(str)
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0

  def not[A](f: A => Boolean): A => Boolean = a => !f(a)

  lazy val isEven: Int => Boolean = not(isOdd)

  isOdd(2)
  isEven(3)


  // изменение самой функции

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = f.curried(a)


  trait Consumer {
    def subscribe(topic: String): Stream[Record]
  }

  case class Record(value: String)

  case class Request()

  object Request {
    def parse(str: String): Request = ???
  }

  /**
   *
   * (Опционально) Реализовать ф-цию, которая будет читать записи Request из топика,
   * и сохранять их в базу
   */
  def createRequestSubscription() = ???


}


/**
 * Реализуем тип Option
 */


object opt {


  class Animal

  class Dog extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутсвие результата
   */

  // 1. invariance
  // 2. covariance  A <- B  Option[A] <- Option[B]
  // 3. contravariance A <- B Option[A] -> Option[B]


  sealed trait Option[+T] {

    def isEmpty: Boolean = this match {
      case Some(v) => false
      case None => true
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty option")
    }


    def map[B](f: T => B): Option[B] = flatMap(t => Option(f(t)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def printIfAny(): Unit = this match {
      case Some(v) => println(v)
      case None => ()
    }

    def zip[B](b: Option[B]): Option[(T, B)] = (this, b) match {
      case (Some(v1), Some(v2)) => Some((v1, v2))
      case _ => None
    }

    def filter(f: T => Boolean): Option[T] = this match {
      case Some(v) => if (f(v)) this else None
      case None => None
    }


  }

  case class Some[V](v: V) extends Option[V]

  case object None extends Option[Nothing] // Any <- Dog

  var o11: Option[Int] = None

  object Option {
    def apply[T](v: T): Option[T] =
      if (v == null) None
      else Some(v)
  }

  val o1: Option[Int] = Option(1)
  o1.isEmpty // false


  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

}

object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
   */


  sealed trait List[+T] {

    def ::[A >: T](v: A): List[A] = List.::(v, this)

    def isEmpty: Boolean = this match {
      case _ => false
      case List.Nil => true
    }

    def mkString(string: String): String = this match {
      case List.::(head, List.Nil) => head + ""
      case List.::(head, tail) => head + string + tail.mkString(string)
      case List.Nil => ""
    }

    def filter(f: T => Boolean): List[T] = this match {
      case List.Nil => List.Nil
      case List.::(head, tail) if f(head) => head :: tail.filter(f)
      case List.::(_, tail) => tail.filter(f)
    }

    def map[A](f: T => A): List[A] = this match {
      case List.::(head: T, tail: List[T]) => List.::(f(head), tail.map(f))
      case List.Nil => List.Nil
    }

    def reverse: List[T] = {
      @tailrec
      def loop(list: List[T], acc: List[T]): List[T] = list match {
        case List.::(head, tail: List[T]) => loop(tail, head :: acc)
        case List.Nil => acc
      }

      loop(this, List.Nil)
    }

    def incList: List[Int] = this.map(_ + 1)

    def flatMap[A](f: T => List[A]): List[A] = this match {
      case List.Nil => List.Nil
      case List.::(head, tail) => f(head) match {
        case List.Nil => tail.flatMap(f)
        case List.::(head, _) => List.::(head, tail.flatMap(f))
      }
    }

    def shoutString: List[String] = this.map(_.toString + "!")


    // def ::


    // map

    // flatMap
  }


  object List {
    case class ::[A](head: A, tail: List[A]) extends List[A]

    case object Nil extends List[Nothing]

    def apply[A](v: A*): List[A] = {
      if (v.isEmpty) Nil else ::(v.head, apply(v.tail: _*))
    }
  }


  /**
   * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
   *
   */


  /**
   * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
   *
   */

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  /**
   *
   * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
   */

  /**
   *
   * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
   */


  /**
   *
   * Реализовать метод filter для списка который будет фильтровать список по некому условию
   */

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */


  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

}