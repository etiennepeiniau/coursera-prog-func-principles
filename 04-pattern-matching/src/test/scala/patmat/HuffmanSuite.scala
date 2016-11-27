package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  trait CodeTables {
    val t1 = ('a', List(0, 0)) ::('b', List(0, 1)) ::('d', List(1)) :: Nil
    val t2 = ('c', List(0, 0)) ::('e', List(0, 1)) ::('f', List(1)) :: Nil
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times on some list") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine with reordering of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5)))
  }

  test("until with some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9)))
  }

  test("create code tree from text") {
    new TestTrees {
      assert(createCodeTree("aabbbdddd".toList) == Fork(Leaf('d', 4), Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), List('d', 'a', 'b'), 9))
    }
  }

  test("decode secret") {
    assert(decodedSecret === "huffmanestcool".toList)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a very short text with french code") {
    assert(decode(frenchCode, encode(frenchCode)("quelquestestspoureviterlesbugs".toList)) === "quelquestestspoureviterlesbugs".toList)
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("addbbadb".toList)) === "addbbadb".toList)
    }
  }

  test("code bits from chars") {
    new CodeTables {
      assert(codeBits(t1)('a') === List(0,0))
    }
  }

  test("merge code tables") {
    new CodeTables {
      assert(mergeCodeTables(t1, t2) === List(('f',List(1, 1)), ('e',List(1, 0, 1)), ('c',List(1, 0, 0)), ('d',List(0, 1)), ('b',List(0, 0, 1)), ('a',List(0, 0, 0))))
    }
  }

  test("convert to code table") {
    new TestTrees {
      assert(convert(t2) === List(('d',List(1)), ('a',List(0, 0)), ('b',List(0, 1))))
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quick encode a very short text with french code") {
    assert(decode(frenchCode, quickEncode(frenchCode)("quelquestestspoureviterlesbugs".toList)) === "quelquestestspoureviterlesbugs".toList)
  }

  test("decode and quick encode a longer text should be identity") {
    new TestTrees {
      assert(decode(t2, quickEncode(t2)("addbbadb".toList)) === "addbbadb".toList)
    }
  }
}
