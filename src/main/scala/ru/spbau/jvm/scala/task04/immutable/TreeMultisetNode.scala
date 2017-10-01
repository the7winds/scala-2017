package ru.spbau.jvm.scala.task04.immutable

case class TreeMultisetNode[+A](v: A, c: Int, left: TreeMultiset[A], right: TreeMultiset[A]) extends TreeMultiset[A] {
  override val size: Int = c + left.size + right.size

  override def foreach(f: (A) => Unit): Unit = {
    Range(0, c).foreach { _ => f(v) }
    left.foreach(f)
    right.foreach(f)
  }

  override def foreach(f: (A, Int) => Unit): Unit = {
    f(v, c)
    left.foreach(f)
    right.foreach(f)
  }

  override def filter(p: (A) => Boolean): TreeMultiset[A] =
    if (p(v)) TreeMultisetNode(v, c, left.filter(p), right.filter(p))
    else left.filter(p) || right.filter(p)

  override def map[B](f: (A) => B): TreeMultiset[B] = {
    val mappedNode = Range(0, c)
      .map { _ => f(v) }
      .foldLeft(TreeMultiset[B]()) { (s, e) => TreeMultiset(s, e) }

    val lm = left.map(f)
    val rm = right.map(f)

    return (mappedNode || lm) || rm
  }

  override def flatMap[B](f: (A) => TreeMultiset[B]): TreeMultiset[B] = {
    val mergedMaps = Range(0, c)
      .map { _ => f(v) }
      .foldLeft(TreeMultiset[B]()) { (s, ms) => s || ms }

    return mergedMaps || left.flatMap(f) || right.flatMap(f)
  }

  override def toList(): List[A] = {
    var b: List[A] = left.toList() ::: right.toList()

    Range(0, c).foreach { _ => b = v :: b }

    return b
  }

  override def find(p: (A) => Boolean): Option[(A, Int)] =
    if (p(v)) Some(v, c) else left.find(p) match {
      case None => right.find(p)
      case w => w
    }

  override def apply[B >: A](e: B): Option[Int] =
    if (e == v) Some(c) else {
      if (e.hashCode() < v.hashCode()) {
        left.apply(e)
      } else {
        right.apply(e)
      }
    }

  override def ||[B >: A](t: TreeMultiset[B]): TreeMultiset[B] = {
    t match {
      case TreeMultisetLeaf() => return this
      case _ =>
    }

    var (root, child) = if (size > t.size) (this, t) else (t, this)

    child.foreach((e, c) => root = TreeMultiset(root, e, c))

    return root
  }

  override def &&[B >: A](t: TreeMultiset[B]): TreeMultiset[B] = {
    t match {
      case TreeMultisetLeaf() => return TreeMultisetLeaf()
      case _ =>
    }

    val (a, b) = if (size > t.size) (t, this) else (this, t)

    var intersection = TreeMultiset[B]()

    a.foreach { (x, c) =>
      apply(x) match {
        case None =>
        case Some(c2) => {
          val ci = Math.min(c2, c)
          if (ci > 0) {
            intersection = TreeMultiset(intersection, x, ci)
          }
        }
      }
    }

    return intersection
  }
}
