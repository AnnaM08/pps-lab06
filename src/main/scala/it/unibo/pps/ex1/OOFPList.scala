package it.unibo.pps.ex1

import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    //foldRight(Nil())(f(_) append _)
    foldLeft(Nil())((list, value) => list.append(f(value)))

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)
  
  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] =
    @tailrec
    def _zipWithValue(acc: List[(A, B)], value: B, list: List[A]): List[(A, B)] = list match
      case h :: t => _zipWithValue((h, value) :: acc, value, t)
      case _ => acc
    val list = _zipWithValue(Nil(), value, this)
    list.reverse()

  def zipWithValueWithFold[B](value: B): List[(A, B)] =
    foldRight[List[(A, B)]](Nil())((h, acc) => (h, value) :: acc)


  def length(): Int = /*this match //NON TAIL
    case _ :: t => 1 + t.length()
    case _ => 0*/
    @tailrec
    def _left(list: List[A], acc: Int): Int = list match
      case _ :: t => _left(t, acc + 1)
      case _ => acc

    _left(this, 0)

  def lengthWithFolder(): Int = foldLeft(0)((acc, _) => acc + 1)


  def indices(): List[Int] =
    @tailrec
    def _indices(acc: List[Int], currentIndex: Int, list: List[A]): List[Int] = list match
      case _ :: t => _indices(currentIndex :: acc, currentIndex + 1, t)
      case _ => acc
    _indices(Nil(), 0, this).reverse()

  def indiciesWithFold(): List[Int] = {
    /*specificare il tipy parameter*/
    val res = foldRight(Nil[Int](), this.length() - 1)((_, acc) =>
      val (currentList, currentIndex) = acc
      (currentIndex :: currentList, currentIndex - 1))

    val (list, _) = res
    list
  }


  def zipWithIndex: List[(A, Int)] =
    @tailrec
    def _zipWithIndex(acc: List[(A, Int)], currrentIndex: Int, list: List[A]): List[(A, Int)] = list match
      case h :: t => _zipWithIndex((h, currrentIndex) :: acc, currrentIndex + 1, t)
      case _ => acc
    _zipWithIndex(Nil(), 0, this).reverse()

  def zipWithIndexWithFold: List[(A, Int)] = {
    val res = foldRight(Nil[(A, Int)](), this.length() - 1)((h, acc) =>
      val (currentList, currentIndex) = acc
      ((h, currentIndex) :: currentList, currentIndex - 1)
    )
    val (list, _) = res
    list
  }


  def partition(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _partition(acc: (List[A], List[A]), list: List[A]): (List[A], List[A]) = {
      val (trueList, falseList) = acc
      list match
      case h :: t if predicate(h) => _partition((h :: trueList, falseList), t)
      case h :: t => _partition((trueList, h :: falseList), t)
      case _ => acc
    }
    val (trueList, falseList) = _partition((Nil(), Nil()), this)
    (trueList.reverse(), falseList.reverse())

  def partitionWithFold(predicate: A => Boolean): (List[A], List[A]) =
    foldRight(Nil[A](), Nil[A]())((h, acc) =>
      val (trueList, falseList) = acc
      if predicate(h) then (h :: trueList, falseList) else (trueList, h :: falseList)
    )


  def span(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _span(acc: (List[A], List[A]), list: List[A], found: Boolean): (List[A], List[A]) ={
      val (trueList, falseList) = acc
      list match
      case h :: t if !found && !predicate(h) =>  _span((trueList, h :: falseList), t, true)
      case h :: t if !found && predicate(h) => _span((h :: trueList, falseList), t, found)
      case h :: t if found => _span((trueList, h :: falseList), t, found)
      case _ => acc
      }

    val (trueList: List[A], falseList: List[A]) = _span((Nil(), Nil()), this, false)
    (trueList.reverse(), falseList.reverse())

  def spanWithFold(predicate: A => Boolean): (List[A], List[A]) = {
    val res = foldLeft((Nil[A](), Nil[A]()), false)((acc, h) =>
      val ((trueList, falseList), found: Boolean) = acc
      if found
      then
        ((trueList, h :: falseList), found)
      else if predicate(h) then
        ((h :: trueList, falseList), found)
        else
          ((trueList, h :: falseList), true)
    )
    val ((trueList, falseList), _) = res
    (trueList.reverse(), falseList.reverse())
  }


  def takeRight(n: Int): List[A] =
    @tailrec
    def _takeRight(acc: List[A], n: Int, list: List[A]): List[A] = list match
      case h :: t if n > 0 => _takeRight(h :: acc, n - 1, t)
      case _ => acc
    _takeRight(Nil(), n, this.reverse())

  def takeRightWithFold(n: Int): List[A] = {
    val res = foldRight(Nil[A](), n)((h, acc) =>
      val (currList, cont) = acc
      if cont > 0 then
        (h :: currList, cont - 1)
      else
        (currList, cont)
    )
    val (list, _) = res
    list
  }


  def collect(predicate: PartialFunction[A, A]): List[A] =
    @tailrec
    def _collect(acc: List[A], list: List[A]): List[A] = list match
      case h :: t if predicate.isDefinedAt(h) => _collect(predicate(h) :: acc, t)
      case _ :: t => _collect(acc, t)
      case _ => acc

    _collect(Nil(), this).reverse()

  def collectWithFold(predicate: PartialFunction[A, A]): List[A] =
    foldRight(Nil())((h, acc) =>
      if predicate.isDefinedAt(h) then
          predicate(h) :: acc
      else
        acc
    )




  def reverse(): List[A] = foldLeft(Nil(): List[A])((acc, h) => h :: acc)
/*
      @tailrec
      def _reverse(acc: List[A], list: List[A]): List[A] = list match
      case h :: t => _reverse(h :: acc, t)
      case _ => acc

      _reverse(Nil(), list)*/

// Factories
object List:

  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = list match
    case ((left, right) :: rest) =>
      val (restLeft, restRight) = unzip(rest)
      (left :: restLeft, right :: restRight)
    case Nil() => (Nil(), Nil())

  def unzipWithFold[A, B](list: List[(A, B)]): (List[A], List[B]) = {
    //valore iniziale poi che cosa si deve fare per ogni elemento
    list.foldRight[(List[A], List[B])]((Nil(), Nil())){
      //parte in cui si accumula
      case ((left, right), (leftList, rightList)) => (left :: leftList, right :: rightList)
    }
  }

  /*posso definire la lista come List[Int]() definire in modo compatto il tipo con */
  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:
  import List.*
  val reference = List(1, 2, 3, 4)
  /*println(unzip(List((1,2), (4,3))))
  println(unzipWithFold(List((1, 2), (4, 3))))*/
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithValueWithFold(10))
  println(reference.length()) // 4
  println(reference.lengthWithFolder())
  println(reference.indices()) // List(0, 1, 2, 3)
  println(reference.indiciesWithFold())
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.zipWithIndexWithFold)
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.partitionWithFold(_ % 2 == 0))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.spanWithFold(_ % 2 != 0))
  println(reference.spanWithFold(_ < 3))
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.takeRightWithFold(3))
  println(reference.collect({case x if x % 2 == 0 => x * 10 })) // List(20, 40)

  println(reference.collectWithFold { case x if x % 2 == 0 => x * 10 }) // List(20, 40)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)*/
  println(reference.collectWithFold { case x if x % 2 == 0 => x + 1 })