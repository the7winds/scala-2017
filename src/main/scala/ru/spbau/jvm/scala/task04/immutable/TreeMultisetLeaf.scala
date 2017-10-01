package ru.spbau.jvm.scala.task04.immutable

case class TreeMultisetLeaf[+A]() extends TreeMultiset[A] {
  override val size: Int = 0

  override def foreach(f: (A) => Unit): Unit = {}

  override def foreach(f: (A, Int) => Unit): Unit = {}

  override def filter(p: (A) => Boolean): TreeMultiset[A] = TreeMultisetLeaf()

  override def map[B](f: (A) => B): TreeMultiset[B] = TreeMultisetLeaf()

  override def flatMap[B](f: (A) => TreeMultiset[B]): TreeMultiset[B] = TreeMultisetLeaf()

  override def toList(): List[A] = List.empty

  override def find(p: (A) => Boolean): None.type = None

  override def apply[B >: A](v: B): None.type = None

  override def ||[B >: A](t: TreeMultiset[B]): TreeMultiset[B] = t

  override def &&[B >: A](t: TreeMultiset[B]): TreeMultiset[B] = TreeMultisetLeaf()
}
