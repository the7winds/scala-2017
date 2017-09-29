package ru.spbau.jvm.scala.task04.mutable

import scala.collection.mutable

class Multiset[T] {
  private val S = 1000
  private val set = new Array[mutable.MutableList[(T, Int)]](S)

  def filter(p: (T, Int) => Boolean): Multiset[T] = {
    val s = new Multiset[T]

    foreach {
      (v, c) => if (p(v, c)) { s.add(v, c) }
    }

    return s
  }

  def map[K](f: (T, Int) => (K, Int)): Multiset[K] = {
    val s = new Multiset[K]

    foreach {
      (v, c1) => {
        val (k, c2) = f(v, c1)
        s.add(k, c2)
      }
    }

    return s
  }

  def flatMap[K](f: (T, Int) => Multiset[K]): Multiset[K] = {
    val s = new Multiset[K]

    foreach {
      (v, c) => {
        s.add(f(v, c))
      }
    }

    return s
  }

  def add(e: T, c: Int): Multiset[T] = {
    var i = e.hashCode() % S
    var l = set{i}

    if (l == null) {
      l = new mutable.MutableList[(T, Int)]()
      set{i} = l
    }

    i = -1
    var j = 0
    var c0 = 0
    l.foreach {
      p => {
        val (v, c) = p
        if (v == e) {
          c0 = c
          i = j
        }
        j += 1
      }
    }

    if (i < 0) {
      l += ((e, c0 + c))
    } else {
      l{i} = (e, c0 + c)
    }

    return this
  }

  def add(s: Multiset[T]): Multiset[T] = {
    s.foreach { (v, c) => add(v, c); }
    return this
  }

  def get(v: T): Int = {
    val i = v.hashCode() % S
    if (set{i} == null) {
      set{i} = new mutable.MutableList[(T, Int)];
    }

    val (_, c) = set{i}.find { p => val (e, _) = p; v == e}.getOrElse((v, 0))
    return c
  }

  def foreach(f: (T, Int) => Unit): Unit = {
    set.filter {
      _ != null
    }.foreach {
      _.foreach {
        p => {
          val (v, c) = p
          f(v, c)
        }
      }
    }
  }

  def find(f: (T, Int) => Boolean): Option[(T, Int)] = {
    foreach { (v, c) => if (f(v, c)) return Some(v, c) }
    return None
  }
}

object Multiset {
  def apply[T](e: T*): Multiset[T] = {
    val s = new Multiset[T]()
    e.foreach { s.add(_, 1) }
    return s
  }

  def unapplySeq[T](e: Multiset[T]): Option[Seq[(T, Int)]] = {
    val builder = Seq.newBuilder[(T, Int)]
    e.foreach { (v, c) => builder += ((v, c))  }
    return Some(builder.result())
  }
}