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
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = this
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
  private def appendRec[S >: T](remaining: RList[S], acc: RList[S]): RList[S] = {
    if (remaining.isEmpty) acc
    else appendRec(remaining.tail, remaining.head :: acc)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    appendRec(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtRec[S >: T](remaining: RList[S], accReversed: RList[S], currentIndex: Int): RList[S] = {
      if(currentIndex == index){
        if(remaining.isEmpty) this
        else appendRec(accReversed, remaining.tail)
      }else{
        if(remaining.isEmpty) this
        else removeAtRec(remaining.tail, remaining.head :: accReversed, currentIndex + 1)
      }
    }
    if(index < 0) this
    else removeAtRec(this, RNil, 0)
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

  //val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  val aLargeList = RList.from(1 to 10)
  println(aLargeList)
  println("8th=" + aLargeList(8))
  println("length=" + aLargeList.length)
  println("reverse=" + aLargeList.reverse)
  println("removeAt(5)=" + aLargeList.removeAt(5))
  println("removeAt(0)=" + aLargeList.removeAt(0))
  println("removeAt(100)=" + aLargeList.removeAt(100))
  println("removeAt(-1)=" + aLargeList.removeAt(-1))
}
