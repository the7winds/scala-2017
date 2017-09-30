package ru.spbau.jvm.scala.task04.immutable

trait TreeMultiset[+A] {
  val size: Int
  def filter(p: A => Boolean): TreeMultiset[A]
  def foreach(f: A => Unit): Unit
  def foreach(f: (A, Int) => Unit): Unit
  def map[B](f: A => B): TreeMultiset[B]
  def flatMap[B](f: A => TreeMultiset[B]): TreeMultiset[B]
  def find(p: A => Boolean): Option[Int]
  def apply[B >: A](v: B): Option[Int]
  def toList(): List[A]
  def ||[B >: A](t: TreeMultiset[B]): TreeMultiset[B]
  def &&[B >: A](t: TreeMultiset[B]): TreeMultiset[B]
}

object TreeMultiset {
  def apply[T](t: TreeMultiset[T], e: T, c0: Int = 1): TreeMultiset[T] = t match {
    case TreeMultisetLeaf() => TreeMultisetNode(e, c0, TreeMultisetLeaf(), TreeMultisetLeaf())
    case TreeMultisetNode(v, c, l, r) => {
      if (e == v) TreeMultisetNode(v, c + c0, l, r)
      else if (e.hashCode() < v.hashCode()) {
        TreeMultisetNode(v, c, TreeMultiset(l, e, c0), r)
      } else {
        TreeMultisetNode(v, c, l, TreeMultiset(r, e, c0))
      }
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
        tmp = (x1 || x2) :: xs
      }
      var tmp2: List[TreeMultiset[T]] = Nil
      while (tmp.nonEmpty) {
        val x1 :: x2 :: xs = tmp
        tmp2 = (x1 || x2) :: tmp2
        tmp = xs
      }
      l = tmp2
    }

    return l.head
  }

  def unapplySeq[T](e: TreeMultiset[T]): Option[Seq[T]] = {
    val builder = Seq.newBuilder[T]
    e.foreach { v => builder += v  }
    return Some(builder.result())
  }
}