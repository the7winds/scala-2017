package ru.spbau.jvm.scala.task04.mutable

import org.scalatest._

import scala.collection.mutable

class HashMultisetTest extends FlatSpec with Matchers {

  "A multiset" should "construct from a value" in {
    val single = HashMultiset(42)
    var cnt = 0

    var ml = mutable.MutableList(1, 2, 3, 4)
    for (x <- single) {
      cnt += 1
    }

    cnt should be (1)
  }

  "A multiset" should "construct big set from sequence" in {
    val set = HashMultiset(1, 2, 3, 2, 3, 3)

    set.apply(1) should be (Some(1))
    set.apply(2) should be (Some(2))
    set.apply(3) should be (Some(3))
  }

  "Function" should "map int to string" in {
    val intSet = HashMultiset(1, 3, 3, 2)
    val stringSet = intSet.map { (v, c) => (v.toString, c) }
    stringSet.apply("1") should be (Some(1))
    stringSet.apply("2") should be (Some(1))
    stringSet.apply("3") should be (Some(2))
  }

  "Function" should "flatmap string set to char set" in {
    val stringSet = HashMultiset("abba", "abba", "dabba")
    val charSet: HashMultiset[Char] = stringSet.flatMap {
      (v, c) => {
        val s = new HashMultiset[Char]()
        v.foreach {
          ch => s.add(ch, c)
        }
        s
      }
    }

    charSet.apply('a') should be (Some(6))
    charSet.apply('b') should be (Some(6))
    charSet.apply('d') should be (Some(1))
    charSet.apply('c') should be (None)
  }

  it should "find element by predicate" in {
    val stringSet = HashMultiset("abba", "abba", "dabba", "zoom")
    stringSet.find((v, _) => v.contains("z")) should be (Some("zoom", 1))
    stringSet.find((v, _) => v == "boom") should be (None)
  }

  "Predicate" should "filter elements" in {
    HashMultiset("abba", "abba", "dabba", "zoom").filter {
      (v, _) => v.contains("a")
    }.find {
      (v, _) => v == "zoom"
    } match {
      case None => ()
      case _ => fail("can't filter elements")
    }
  }

  "A multiset" should "match" in {
    HashMultiset("abba", "abba", "dabba", "zoom") match {
      case HashMultiset(_, _, _, _) => ()
      case _ => fail("can't match set from 4 elements")
    }

    HashMultiset("zoom") match {
      case HashMultiset(_) => ()
      case _ => fail("can't match set from 1 element")
    }

    HashMultiset() match {
      case HashMultiset() => ()
      case _ => fail("can't match empty set")
    }
  }

  "Sets" should "intersects" in {
    val s1 = HashMultiset(1, 2, 3, 3, 4, 5)
    val s2 = HashMultiset(1, 3, 3, 6)
    val s = s1 && s2

    s.apply(1) should be (Some(1))
    s.apply(2) should be (None)
    s.apply(3) should be (Some(2))
    s.apply(4) should be (None)
    s.apply(5) should be (None)
    s.apply(6) should be (None)
  }
}
