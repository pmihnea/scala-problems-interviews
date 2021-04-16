package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  val optionValue: Option[T] = if (isEmpty) None else Some(value)

  /*
  Easy problems
  */
  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int

  /*
  Medium
   */
  // the number of nodes in the tree
  def size: Int

  def collectNodes(level: Int): List[BTree[T]]

  def mirror: BTree[T]

  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  def isSymmetrical: Boolean
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List()

  override def leafCount: Int = 0

  override val size: Int = 0

  override def collectNodes(level: Int): List[BTree[Nothing]] = List()

  override def mirror: BTree[Nothing] = this

  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  override def isSymmetrical: Boolean = true
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

  override val size: Int = 1 + left.size + right.size
  /*
    override def size: Int = {
      @tailrec
      def sizeTailRec(remaining: List[BTree[T]], result: Int) : Int = {
        if(remaining.isEmpty) result
        else if(remaining.head.isEmpty) sizeTailRec(remaining.tail, result)
        else if(remaining.head.isLeaf) sizeTailRec(remaining.tail, result + 1)
        else {
          val node = remaining.head
          sizeTailRec(node.left :: node.right :: remaining.tail, result + 1)
        }
      }
      sizeTailRec(List(this), 0)
    }
  */

  override def collectNodes(level: Int): List[BTree[T]] = {
    @tailrec
    def collectTailRecDFS(remaining: List[(BTree[T], Int)], acc: List[BTree[T]]): List[BTree[T]] = {
      if (remaining.isEmpty) acc
      else {
        val (node, nodeLevel) = remaining.head
        if (node.isEmpty) collectTailRecDFS(remaining.tail, acc)
        else {
          if (nodeLevel == level) collectTailRecDFS(remaining.tail, node :: acc)
          else collectTailRecDFS((node.left, nodeLevel + 1) :: (node.right, nodeLevel + 1) :: remaining.tail, acc)
        }
      }
    }

    @tailrec
    def collectTailRecBFS(nodeLevel: Int, nodes: List[BTree[T]]): List[BTree[T]] = {
      if (nodes.isEmpty) List()
      else if (nodeLevel == level) nodes
      else {
        val newNodes = for {
          node <- nodes
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child
        collectTailRecBFS(nodeLevel + 1, newNodes)
      }
    }

    if (level < 0) List()
    //else collectTailRecDFS(List((this, 0)), List())
    else collectTailRecBFS(0, List(this))
  }

  /*  override def mirror: BTree[T] =
        if(isLeaf) this
        else BNode(this.value, this.right.mirror, this.left.mirror)*/

  override def mirror: BTree[T] = {
    @tailrec
    def mirrorTailRec(todo: List[BTree[T]], visited: Set[Int], done: List[BTree[T]]): BTree[T] = {
      if (todo.isEmpty) done.head
      else {
        if (todo.head.isEmpty || todo.head.isLeaf) {
          mirrorTailRec(todo.tail, visited, todo.head :: done)
        } else if (!visited(System.identityHashCode(todo.head))) {
          mirrorTailRec(todo.head.left :: todo.head.right :: todo, visited + System.identityHashCode(todo.head), done)
        } else {
          val newDone = BNode(todo.head.value, done.head, done.tail.head) :: done.tail.tail //left swaps here with right
          mirrorTailRec(todo.tail, visited, newDone)
        }
      }
    }

    mirrorTailRec(List(this), Set(), List())
  }

  /*  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
      if (this.isLeaf) that.isLeaf
      else if (left.isEmpty) that.left.isEmpty && right.sameShapeAs(that.right)
      else if (right.isEmpty) that.right.isEmpty && left.sameShapeAs(that.left)
      else left.sameShapeAs(that.left) && right.sameShapeAs(that.right)
    }*/

  /*
  Complexity : runtime O(min(N1,N2)), space O(min(N1,N2))
   */
  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    @tailrec
    def sameShapeAsTailRec(list1: List[BTree[S]], list2: List[BTree[S]]): Boolean = {
      if (list1.isEmpty) list2.isEmpty
      else if (list2.isEmpty) list1.isEmpty
      else {
        val head1 = list1.head
        val head2 = list2.head
        if (head1.isEmpty != head2.isEmpty
          || head1.isLeaf != head2.isLeaf) false
        else if (head1.isEmpty && head2.isEmpty
          || head1.isLeaf && head2.isLeaf) sameShapeAsTailRec(list1.tail, list2.tail)
        else sameShapeAsTailRec(
          head1.left :: head1.right :: list1.tail,
          head2.left :: head2.right :: list2.tail
        )
      }
    }

    sameShapeAsTailRec(List(this), List(that))
  }

  override def isSymmetrical: Boolean = {
    this.sameShapeAs(this.mirror)
  }
}

object BinaryTreeProblems extends App {
  /*
       7
       |
      ---
    /     \
    1      4
    |      |
  /   \   / \
  2   3  5  6
        /
       8
      /
     9
   */
  val tree1 = BNode(1,
    BNode(2, BEnd, BEnd),
    BNode(3, BEnd, BEnd)
  )
  val tree4 = BNode(8,
    BNode(9, BEnd, BEnd),
    BEnd
  )
  val tree2 = BNode(4,
    BNode(5, tree4, BEnd),
    BNode(6, BEnd, BEnd)
  )

  val tree3 = BNode(7, tree1, tree2)

  private val bigTree = (1 to 10000).foldLeft[BTree[Int]](BEnd) { case (child, value) => BNode(value, child, BEnd) }

  println("collectLeaves :")
  println(tree3.collectLeaves)
  println("leafCount :")
  println(tree3.leafCount)
  println("size :")
  println(tree3.size)

  println("collectNodes :")
  println(tree3.collectNodes(0).map(_.value)) //7
  println(tree3.collectNodes(1).map(_.value)) //4,1
  println(tree3.collectNodes(2).map(_.value)) //2,3,5,6
  println(tree3.collectNodes(3).map(_.value)) //8
  println(tree3.collectNodes(4).map(_.value)) //9
  println(bigTree.collectNodes(10000 - 1).size) //1

  println("mirror :")
  println(tree1.mirror)
  println(tree2.mirror)
  println(tree3.mirror)
  println(tree3.mirror)
  private val bigTreeMirrored: BTree[Int] = bigTree.mirror
  println(bigTree.optionValue, bigTree.left.optionValue, bigTree.right.optionValue)
  println(bigTreeMirrored.optionValue, bigTreeMirrored.left.optionValue, bigTreeMirrored.right.optionValue)

  println("sameShapeAs: ")
  println(tree3.sameShapeAs(tree3)) // true
  println(tree3.sameShapeAs(tree3.mirror)) // false
  println(tree3.sameShapeAs(tree2)) // false
  println(bigTree.sameShapeAs(bigTreeMirrored)) // false
  println(bigTree.sameShapeAs(bigTree)) // true

  println("isSymmetrical: ")
  println(tree1.isSymmetrical)
  println(tree3.isSymmetrical)

}
