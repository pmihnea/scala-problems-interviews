package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  /*
  Easy problems
  */
  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0
}

case class BNode[+T](override val value: T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  /*
  Easy problems
  */
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    // not tail recursive
    def collectLeavesRec(tree: BTree[T], acc: List[BTree[T]]): List[BTree[T]] = {
      if (tree.isEmpty) acc
      else if (tree.isLeaf) tree :: acc
      else collectLeavesRec(tree.right, collectLeavesRec(tree.left, acc))
    }

    @tailrec
    def collectLeavesTailRec(remaining: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
      if (remaining.isEmpty) acc
      else {
        val node = remaining.head
        if (node.isEmpty) collectLeavesTailRec(remaining.tail, acc)
        else if (node.isLeaf) collectLeavesTailRec(remaining.tail, node :: acc)
        else collectLeavesTailRec(node.left :: node.right :: remaining.tail, acc)
      }
    }

    collectLeavesTailRec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length
}

object BinaryTreeProblems extends App {

  val tree1 = BNode(1,
    BNode(2, BEnd, BEnd),
    BNode(3, BEnd, BEnd)
  )
  val tree2 = BNode(4,
    BNode(5, BEnd, BEnd),
    BNode(6, BEnd, BEnd)
  )
  val tree3 = BNode(7, tree1, tree2)

  println(tree3.collectLeaves)
  println(tree3.leafCount)
}
