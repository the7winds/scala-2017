package ru.spbau.jvm.scala.task04.immutable

import org.scalatest._

class TreeHashMultisetTest extends FlatSpec with Matchers {
  "A multiset" should "construct from a value" in {
    val single = TreeMultiset(42)
    var cnt = 0

    single.size should be (1)
    single.apply(42) should be (Some(1))
  }

  "A multiset" should "construct big set from sequence" in {
    val set = TreeMultiset('3', '1', '2', '3', '2', '3')

    set.apply('1') should be (Some(1))
    set.apply('2') should be (Some(2))
    set.apply('3') should be (Some(3))
  }


  "A multiset" should "match" in {
    TreeMultiset("abba", "abba", "dabba", "zoom") match {
      case TreeMultiset(_, _, _, _) => ()
      case _ => fail("can't match set")
    }

    TreeMultiset("zoom") match {
      case TreeMultiset(_) => ()
      case _ => fail("can't match set")
    }

    TreeMultiset() match {
      case TreeMultiset() => ()
      case _ => fail("can't match set")
    }
  }

  "Function" should "map int to string" in {
    val set = TreeMultiset(1, 3, 2, 3, 2, 3).map { _.toString }
    set.apply("1") should be (Some(1))
    set.apply("2") should be (Some(2))
    set.apply("3") should be (Some(3))
  }

  "Function" should "flatmap string set to char set" in {
    val stringSet = TreeMultiset("abba", "abba", "dabba")
    val charSet: TreeMultiset[Char] = stringSet.flatMap {
      v => v.foldLeft(TreeMultiset[Char]()) { (s, ch) => TreeMultiset(s, ch) }
    }

    charSet.apply('a') should be (Some(6))
    charSet.apply('b') should be (Some(6))
    charSet.apply('d') should be (Some(1))
    charSet.apply('c') should be (None)
  }

  it should "find element by predicate" in {
    val stringSet = TreeMultiset("abba", "abba", "dabba", "zoom")
    stringSet.find(_.contains("z")) should be (Some("zoom", 1))
    stringSet.find(_ == "boom") should be (None)
  }

  "Predicate" should "filter elements" in {
    TreeMultiset("abba", "abba", "dabba", "zoom").filter {
      _.contains("a")
    }.find {
      _ == "zoom"
    } match {
      case None => ()
      case _ => fail("can't filter elements")
    }
  }

  it should "iterate over set" in {
    var size = 0
    for (_ <- TreeMultiset(1, 2, 1, 3, 4, 5)) {
      size += 1
    }

    size should be (6)
  }

  "Sets" should "intersects" in {
    val s1 = TreeMultiset(1, 2, 3, 3, 4, 5)
    val s2 = TreeMultiset(1, 3, 3, 6)
    val s = s1 && s2

    s.apply(1) should be (Some(1))
    s.apply(2) should be (None)
    s.apply(3) should be (Some(2))
    s.apply(4) should be (None)
    s.apply(5) should be (None)
    s.apply(6) should be (None)
  }
}
