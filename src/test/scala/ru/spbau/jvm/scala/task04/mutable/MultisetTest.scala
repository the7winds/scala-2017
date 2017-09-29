package ru.spbau.jvm.scala.task04.mutable

import org.scalatest._

class MultisetTest extends FlatSpec with Matchers {

  "A multiset" should "construct from a value" in {
    val single = Multiset(42)
    var cnt = 0

    single.foreach { (_, c) => cnt += c }

    cnt should be (1)
  }

  "A multiset" should "construct big set from sequence" in {
    val set = Multiset(1, 2, 1, 3, 3)

    set.get(1) should be (2)
    set.get(2) should be (1)
    set.get(3) should be (2)
  }

  "A multiset" should "deconstruct in sequence" in {
    val set = Multiset('1', '2', '3')

    set match {
      case Multiset(_, _, _) => ()
    }
  }

  "Function" should "map int to string" in {
    val intSet = Multiset(1, 3, 3, 2)
    val stringSet = intSet.map { (v, c) => (v.toString, c) }
    stringSet.get("1") should be (1)
    stringSet.get("2") should be (1)
    stringSet.get("3") should be (2)
  }

  "Function" should "flatmap string set to char set" in {
    val stringSet = Multiset("abba", "abba", "dabba")
    val charSet: Multiset[Char] = stringSet.flatMap {
      (v, c) => {
        val s = new Multiset[Char]()
        v.foreach {
          ch => s.add(ch, c)
        }
        s
      }
    }

    charSet.get('a') should be (6)
    charSet.get('b') should be (6)
    charSet.get('d') should be (1)
    charSet.get('c') should be (0)
  }

  it should "find element by predicate" in {
    val stringSet = Multiset("abba", "abba", "dabba", "zoom")
    val Some((_, _)) = stringSet.find((v, _) => v == "zoom")
    val None = stringSet.find((v, _) => v == "zzoom")
  }

  "Predicate" should "filter elements" in {
    Multiset("abba", "abba", "dabba", "zoom").filter {
      (v, _) => v.contains("a")
    }.find {
      (v, _) => v == "zoom"
    } match {
      case None => ()
    }
  }
}
