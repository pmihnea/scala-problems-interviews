package com.rockthejvm.trees

import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T

  def left: BTree[T]

  def right: BTree[T]

  def isEmpty: Boolean

  val optionValue: Option[T] = if(isEmpty) None else Some(value)

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
    // [a,E,E] -> this
    // [a,b,c] -> [a,c,b]
    case class Frame[+T](root: BTree[T], left: Option[BTree[T]] = Option.empty, result: Option[BTree[T]] = Option.empty)

    def unwindStack(result: BTree[T], stack: List[Frame[T]]): List[Frame[T]] = {
      assert(stack.nonEmpty)
      //unwind the stack and place the new result
      val newFrameWithResult = stack.head.copy(result = Some(result))
      newFrameWithResult :: stack.tail
    }

    /*
    Complexity: runtime O(N), space O(N)
     */
    @tailrec
    def mirrorTailRec(stack: List[Frame[T]] = List()): BTree[T] = {
      assert(stack.nonEmpty)

      val frame = stack.head
      val newStack = stack.tail

      if (frame.result.isDefined) {
        // after stack unwinding
        // use the result in the current frame
        if (!frame.left.isDefined) {
          // use the result as left in the current frame
          val newFrameWithLeft = frame.copy(left = frame.result, result = None)
          mirrorTailRec(newFrameWithLeft :: newStack)
        } else {
          // use result as right in the current frame
          // create a node as a new result
          val newResult = BNode(frame.root.value, frame.left.get, frame.result.get)
          if (newStack.isEmpty) newResult // final result
          else {
            //unwind the stack and place the new result
            mirrorTailRec(unwindStack(newResult, newStack))
          }
        }
      } else {
        // no result so no stack unwinding
        if (frame.root.isEmpty || frame.root.isLeaf) {
          // finish the node
          val newResult = frame.root
          //unwindStack(newResult, newStack)
          if (newStack.isEmpty) newResult // final result
          else {
            //unwind the stack and place the new result
            mirrorTailRec(unwindStack(newResult, newStack))
          }
        } else if (!frame.left.isDefined) {
          // calculate left as right.mirror
          // add to the stack the right as root to be calculated
          mirrorTailRec(Frame(root = frame.root.right) :: stack)
        } else {
          // calculate right as left.mirror
          // add to the stack the left as root to be calculated
          mirrorTailRec(Frame(root = frame.root.left) :: stack)
        }
      }
    }

    mirrorTailRec(List(Frame(root = this)))
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

  println("mirror :")
  println(tree1.mirror)
  println(tree2.mirror)
  println(tree3.mirror)
  println(tree3.mirror)
  private val bigTree = (1 to 10000).foldLeft[BTree[Int]](BEnd) { case (child, value) => BNode(value, child, BEnd) }
  private val bigTreeMirrored: BTree[Int] = bigTree.mirror
  println(bigTree.optionValue, bigTree.left.optionValue, bigTree.right.optionValue)
  println(bigTreeMirrored.optionValue, bigTreeMirrored.left.optionValue, bigTreeMirrored.right.optionValue)
}
