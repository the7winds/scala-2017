package ru.spbau.jvm.scala.task04.immutable

case class TreeMultisetNode[+A](v: A, c: Int, left: TreeMultiset[A], right: TreeMultiset[A]) extends TreeMultiset[A] {
  override val size: Int = c + left.size + right.size

  override def foreach(f: (A) => Unit): Unit = {
    Range(0, c).foreach { _ => f(v) }
    left.foreach(f)
    right.foreach(f)
  }

  override def filter(p: (A) => Boolean): TreeMultiset[A] =
    if (p(v)) TreeMultisetNode(v, c, left.filter(p), right.filter(p))
    else TreeMultiset.merge(left.filter(p), right.filter(p))

  override def map[B](f: (A) => B): TreeMultiset[B] = {
    val mappedNode = Range(0, c)
      .map { _ => f(v) }
      .foldLeft(TreeMultiset[B]()) { (s, e) => TreeMultiset(e, s) }

    return TreeMultiset.merge(mappedNode, TreeMultiset.merge(left.map(f), right.map(f)))
  }

  override def flatMap[B](f: (A) => TreeMultiset[B]): TreeMultiset[B] = {
    val mergedMaps = Range(0, c)
      .map { _ => f(v) }
      .foldLeft(TreeMultiset[B]()) { (s, ms) => TreeMultiset.merge(s, ms) }

    return TreeMultiset.merge(mergedMaps, TreeMultiset.merge(left.flatMap(f), right.flatMap(f)))
  }

  override def toList(): List[A] = {
    var b: List[A] = left.toList() ::: right.toList()

    Range(0, c).foreach { _ => b = v :: b }

    return b
  }

  override def find(p: (A) => Boolean): Option[Int] =
    if (p(v)) Some(c) else left.find(p) match {
      case None => right.find(p)
      case w => w
    }

  override def get(e: Any): Option[Int] =
    if (e == v) Some(c) else {
      if (e.hashCode() < v.hashCode()) {
        left.get(e)
      } else {
        right.get(e)
      }
    }
}
