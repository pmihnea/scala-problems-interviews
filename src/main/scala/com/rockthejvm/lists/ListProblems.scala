package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.util.Random

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

  /**
    * Medium
    */
  // run-length encoding
  def rle: RList[(T, Int)]

  def duplicateEach(k: Int): RList[T]

  def rotate(k: Int): RList[T]

  def sample(k: Int): RList[T]

  def insertionSort[S >: T](ordering: Ordering[S]): RList[S]

  def mergeSort[S >: T](ordering: Ordering[S]): RList[S]
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

  override def rle: RList[(Nothing, Int)] = RNil

  override def duplicateEach(k: Int): RList[Nothing] = RNil

  override def rotate(k: Int): RList[Nothing] = RNil

  override def sample(k: Int): RList[Nothing] = RNil

  override def insertionSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil

  override def mergeSort[S >: Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  /**
    * Complexity: O(N) - N is the number of elements of the remaining list
    */
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

  /*
    * Complexity: O(N) - where N is the number of the elements of the list
    */
  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def mapRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else mapRec(remaining.tail, f(remaining.head) :: acc)
    }

    mapRec(this, RNil)
  }

  /*
    * Complexity: O(N * M) - where N is the number of the elements of the list and
    * M is the average number of elements of the f(x) list, where x is an element of the list
    */
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def flatMapRec(remaining: RList[T], acc: RList[S]): RList[S] = {
      if (remaining.isEmpty) acc.reverse
      else flatMapRec(remaining.tail, concatReversedRec[S](f(remaining.head), acc))
    }

    flatMapRec(this, RNil)
  }

  /*
  * Complexity: O(N)
   */
  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterRec(remaining: RList[T], accReversed: RList[T]): RList[T] = {
      if (remaining.isEmpty) accReversed.reverse
      else if (f(remaining.head)) filterRec(remaining.tail, remaining.head :: accReversed)
      else filterRec(remaining.tail, accReversed)
    }

    filterRec(this, RNil)
  }

  /*
  Complexity: O(N)
   */
  override def rle: RList[(T, Int)] = {
    @tailrec
    def rleRec(remaining: RList[T], currentElement: T, currentOccurrences: Int, acc: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty) ((currentElement, currentOccurrences) :: acc).reverse
      else if (remaining.head == currentElement) rleRec(remaining.tail, currentElement, currentOccurrences + 1, acc)
      else rleRec(remaining.tail, remaining.head, 1, (currentElement, currentOccurrences) :: acc)
    }

    rleRec(tail, head, 1, RNil)
  }

  /*
  [1,2,3].duplicateEach(3) = duplicateRec([1,2,3], 0, [])
  = dupR([1,2,3], 1, [1])
  = dupR([1,2,3], 2, [1,1])
  = dupR([1,2,3], 3, [1,1,1])
  = dupR([2,3], 0, [1,1,1]) = ...
  = dupR([], 0, [3,3,3,2,2,2,1,1,1])
  = [1,1,1,2,2,2,3,3,3]

  Complexity: O(N * k)
   */
  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateRec(remaining: RList[T], nAddedDuplications: Int, acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc.reverse
      else if (nAddedDuplications == k) duplicateRec(remaining.tail, 0, acc)
      else duplicateRec(remaining, nAddedDuplications + 1, remaining.head :: acc)
    }

    duplicateRec(this, 0, RNil)
  }

  /*
  Complexity: O(max(N,k))
   */
  override def rotate(k: Int): RList[T] = {
    /*
    [1,2,3,4,5,6,7].rotate(3) =
    = rRec([1,2,3,4,5,6,7], 0,[])
    = rRec([2,3,4,5,6,7],1,[1]) = ...
    = rRec([4,5,6,7],3,[3,2,1])
    = [4,5,6,7] ++ [3,2,1].reverse
     */
    @tailrec
    def rotateRec(remaining: RList[T], counter: Int, acc: RList[T]): RList[T] = {
      if (counter == k) {
        remaining ++ acc.reverse
      } else if (remaining.isEmpty) {
        rotateRec(this, counter, RNil)
      } else {
        rotateRec(remaining.tail, counter + 1, remaining.head :: acc)
      }
    }

    if (k <= 0) {
      this
    } else {
      rotateRec(this, 0, RNil)
    }
  }

  /*
  Complexity: O(N*k)
   */
  override def sample(k: Int): RList[T] = {
    val n = this.length
    val random = new Random(System.currentTimeMillis());
    @tailrec
    def sampleRec(counter: Int, acc: RList[T]): RList[T] = {
      if (counter > k) acc
      else sampleRec(counter + 1, this.apply(random.nextInt(n)) :: acc)
    }

    def sampleElegant(): RList[T] =
      RList.from((1 to k).map(_ => random.nextInt(n)).map(this (_)))

    if (k < 0) RNil
    else sampleElegant()
  }

  override def insertionSort[S >: T](ordering: Ordering[S]): RList[S] = {
    /*
    insertRec(4, [1,2,5,6], [])
    = iRec(4, [2,5,6],[1])
    = iRec(4, [5,6], [2,1] = concatReverseRec([2,1], [4,5,6]) = [1,2,4,5,6]

    insertRec(4, [1,2,3], []) =
    = iRec(4, [2,3],[1])
    = iRec(4, [3], [2,1])
    = iRec(4, [], [3,2,1]) = [4,3,2,1].reverse
    = [1,2,3,4]
     */
    /*
    Complexity: O(N) - 2xN - one for traversal one for reversing,
      where N is the number of elements of the remaining list
     */
    @tailrec
    def insertRec(elem: S, beforeSorted: RList[S], afterSortedReversed: RList[S]): RList[S] = {
      if (beforeSorted.isEmpty) (elem :: afterSortedReversed).reverse
      else if (ordering.lt(elem, beforeSorted.head)) concatReversedRec(afterSortedReversed, elem :: beforeSorted)
      else insertRec(elem, beforeSorted.tail, beforeSorted.head :: afterSortedReversed)
    }

    /*
    foreach([3,1,4,2], RNil) =
    = fRec([1,4,2], insertRec(3, RNil, RNil)) = fRec([1,4,2], [3])
    = fRec([4,2] , insertRec(1, [3], RNil)) = fRec([4,2], [1,3])
    = fRec([2], insertRec(4, [1,3], RNil) = fRec([2], [1,3,4])
    = fRec([], insertRec(2, [1,3,4] ,RNil) = fRec([], [1,2,3,4])
    = [1,2,3,4]
     */
    /*
    Complexity: O(1+2+3+..+N) = O(N^2)
     */
    @tailrec
    def foreachRec(remaining: RList[T], result: RList[S]): RList[S] = {
      if (remaining.isEmpty) result
      else {
        foreachRec(remaining.tail, insertRec(remaining.head, result, RNil))
      }
    }

    foreachRec(this, RNil)
  }

  /*
  Complexity O(N * log N)
   */
  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {
    @tailrec
    def mergeRec(left: RList[S], right: RList[S], acc: RList[S]): RList[S] = {
      if (left.isEmpty && right.isEmpty) acc.reverse
      else if (left.isEmpty) mergeRec(RNil, RNil, concatReversedRec(right, acc))
      else if (right.isEmpty) mergeRec(RNil, RNil, concatReversedRec(left, acc))
      else if (ordering.lt(left.head, right.head)) mergeRec(left.tail, right, left.head :: acc)
      else mergeRec(left, right.tail, right.head :: acc)
    }

    @tailrec
    def mergeSortRec(inPartitions: RList[RList[S]], outPartitions: RList[RList[S]]): RList[S] = {
      if (inPartitions.isEmpty) {
        if (outPartitions.isEmpty) RNil
        else if (outPartitions.tail.isEmpty) outPartitions.head
        else mergeSortRec(outPartitions, RNil)
      } else if (inPartitions.tail.isEmpty) {
        if(outPartitions.isEmpty) inPartitions.head
        else mergeSortRec(inPartitions.head :: outPartitions, RNil)
      }
      else mergeSortRec(inPartitions.tail.tail, mergeRec(inPartitions.head, inPartitions.tail.head, RNil) :: outPartitions)
    }

    val partitions = this.map((_ :: RNil))
    mergeSortRec(partitions, RNil)
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

  test(RNil)
  test(1 :: 2 :: 3 :: 4 :: RNil)
  test(3 :: 1 :: 4 :: 2 :: RNil)
  test(3 :: 1 :: 4 :: 2 :: 0 :: RNil)
  test(2 :: RNil)
  test(RList.from(Range.inclusive(1, 10000)))
  test(RList.from(Range.inclusive(10000, 1, -1)))

  private def test(aList: RList[Int]) = {
    val startTime = System.currentTimeMillis()
    val aListSorted = aList.mergeSort(Ordering[Int])
    val duration = System.currentTimeMillis() - startTime
    println("#######")
    println("initial = " + aList)
    println("sorted = " + aListSorted)
    println(s"duration = $duration ms ")
  }
}
