package ru.spbau.jvm.scala.task04.immutable

import org.scalatest._

class TreeMultisetTest extends FlatSpec with Matchers {
  "A multiset" should "construct from a value" in {
    val single = TreeMultiset(42)
    var cnt = 0

    single.size should be (1)
    single.get(42) should be (Some(1))
  }

  "A multiset" should "construct big set from sequence" in {
    val set = TreeMultiset('3', '1', '2', '3', '2', '3')

    set.get('1') should be (Some(1))
    set.get('2') should be (Some(2))
    set.get('3') should be (Some(3))
  }

  "A multiset" should "deconstruct in sequence" in {
    val set = TreeMultiset('1', '2', '3')

    set match {
      case TreeMultiset(_, _, _) => ()
    }
  }

  "Function" should "map int to string" in {
    val intSet = TreeMultiset(1, 3, 3, 2)
    val stringSet = intSet.map { _.toString }
    stringSet.get("1") should be (Some(1))
    stringSet.get("2") should be (Some(1))
    stringSet.get("3") should be (Some(2))
  }

  "Function" should "flatmap string set to char set" in {
    val stringSet = TreeMultiset("abba", "abba", "dabba")
    val charSet: TreeMultiset[Char] = stringSet.flatMap {
      v => v.foldLeft(TreeMultiset[Char]()) { (s, ch) => TreeMultiset.apply(ch, s) }
    }

    charSet.get('a') should be (Some(6))
    charSet.get('b') should be (Some(6))
    charSet.get('d') should be (Some(1))
    charSet.get('c') should be (None)
  }

  it should "find element by predicate" in {
    val stringSet = TreeMultiset("abba", "abba", "dabba", "zoom")
    val Some(1) = stringSet.find(_ == "zoom")
    val None = stringSet.find(_ == "boom")
  }

  "Predicate" should "filter elements" in {
    TreeMultiset("abba", "abba", "dabba", "zoom").filter {
      _.contains("a")
    }.find {
      _ == "zoom"
    } match {
      case None => ()
    }
  }

  it should "iterate over set" in {
    var size = 0
    for (_ <- TreeMultiset(1, 2, 1, 3, 4, 5)) {
      size += 1
    }

    size should be (6)
  }
}
