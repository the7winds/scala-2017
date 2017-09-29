package ru.spbau.jvm.scala.task04.immutable

trait TreeMultiset[+A] {
  val size: Int
  def filter(p: A => Boolean): TreeMultiset[A]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): TreeMultiset[B]
  def flatMap[B](f: A => TreeMultiset[B]): TreeMultiset[B]
  def find(p: A => Boolean): Option[Int]
  def get(v: Any): Option[Int]
  def toList(): List[A]
}

object TreeMultiset {
  def merge[T](a: TreeMultiset[T], b: TreeMultiset[T]): TreeMultiset[T] = {
    a match {
      case TreeMultisetLeaf() => return b
      case _ => ()
    }

    b match {
      case TreeMultisetLeaf() => return a
      case _ => ()
    }

    var (root, child) = if (a.size > b.size) (a, b) else (b, a)

    child.foreach(e => root = TreeMultiset(e, root))

    return root
  }

  def apply[T](e: T, t: TreeMultiset[T]): TreeMultiset[T] = {
    t match {
      case TreeMultisetNode(v, c, l, r) => {
        if (e == v) return TreeMultisetNode(v, c + 1, l, r)
        if (e.hashCode() < v.hashCode()) {
          return TreeMultisetNode(v, c, TreeMultiset(e, l), r)
        } else {
          return TreeMultisetNode(v, c, l, TreeMultiset(e, r))
        }
      }
      case TreeMultisetLeaf() => return TreeMultisetNode(e, 1, TreeMultisetLeaf(), TreeMultisetLeaf())
    }
  }

  def apply[T](vs: T*): TreeMultiset[T] = {
    if (vs == Nil) {
      return TreeMultisetLeaf()
    }

    var l: List[TreeMultiset[T]] = vs.toList.map(TreeMultisetNode(_, 1, TreeMultisetLeaf(), TreeMultisetLeaf()))

    while (l.size > 1) {
      var tmp = l
      if (tmp.size % 2 == 1) {
        val x1 :: x2 :: xs = tmp
        tmp = TreeMultiset.merge(x1, x2) :: xs
      }
      var tmp2: List[TreeMultiset[T]] = Nil
      while (tmp.nonEmpty) {
        val x1 :: x2 :: xs = tmp
        tmp2 = TreeMultiset.merge(x1, x2) :: tmp2
        tmp = xs
      }
      l = tmp2
    }

    return l.head
  }

  def unapplySeq[T](tree: TreeMultiset[T]): Option[Seq[T]] = {
    tree match {
      case TreeMultisetNode(_, _, _, _) => Some(tree.toList().seq)
      case _ => None
    }
  }
}