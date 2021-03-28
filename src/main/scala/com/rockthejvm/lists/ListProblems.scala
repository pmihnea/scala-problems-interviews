package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T

  def tail: RList[T]

  def isEmpty: Boolean

  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  def apply(index: Int): T

  def length: Int

  def reverse: RList[T]

  def ++[S >: T](anotherList: RList[S]): RList[S]

  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]

  def flatMap[S](f: T => RList[S]): RList[S]

  def filter(f: T => Boolean): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = RNil

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    @tailrec def getKthElem(k: Int, remaining: RList[T]): T = {
      if (k == index) remaining.head
      else getKthElem(k + 1, remaining.tail)
    }

    if (index < 0) throw new NoSuchElementException
    else getKthElem(0, this)
  }

  override def length: Int = {
    @tailrec
    def lengthRec(remaining: RList[T], acc: Int): Int = {
      if (remaining.isEmpty) acc
      else lengthRec(remaining.tail, acc + 1)
    }

    lengthRec(this, 0);
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseRec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else reverseRec(remaining.tail, remaining.head :: acc)
    }

    reverseRec(this, RNil)
  }

  @tailrec
  private def concatReversedRec[S](remaining: RList[S], acc: RList[S]): RList[S] = {
    if (remaining.isEmpty) acc
    else concatReversedRec(remaining.tail, remaining.head :: acc)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    concatReversedRec(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtRec[S >: T](remaining: RList[S], acc: RList[S], currentIndex: Int): RList[S] = {
      if (remaining.isEmpty) this
      else if (currentIndex == index) concatReversedRec(acc, remaining.tail)
      else removeAtRec(remaining.tail, remaining.head :: acc, currentIndex + 1)
    }

    if (index < 0) this
    else removeAtRec(this, RNil, 0)
  }

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else mapRec(remaining.tail, f(remaining.head) :: acc)
    }

    mapRec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else flatMapRec(remaining.tail, concatReversedRec[S](f(remaining.head), acc))
    }

    flatMapRec(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterRec(remaining: RList[T], accReversed: RList[T]): RList[T] = {
      if (remaining.isEmpty) accReversed.reverse
      else if (f(remaining.head)) filterRec(remaining.tail, remaining.head :: accReversed)
      else filterRec(remaining.tail, accReversed)
    }

    filterRec(this, RNil)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def fromRec(iterable: Iterable[T], acc: RList[T]): RList[T] = {
      if (iterable.isEmpty) acc
      else fromRec(iterable.tail, iterable.head :: acc)
    }

    fromRec(iterable, RNil).reverse
  }
}

object ListProblems extends App {

  val aLargeList = RList.from(1 to 10)
  val anotherList = RList.from(11 to 15)
  println(aLargeList)
  println(aLargeList ++ anotherList)
  println("map(x*x)=" + aLargeList.map(x => x * x))
  println("flatMap(x*x, x*x+1)=" + aLargeList.flatMap(x => x * x :: x * x + 1 :: RNil))
  println("filter(x%2==0)=" + aLargeList.filter(x => x % 2 == 0))
}
