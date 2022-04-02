package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
   * If you insert any two elements into an empty heap, 
   * finding the minimum of the resulting heap should get 
   * the smallest of the two elements back. 
   */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val expected = if (a < b) a else b
    findMin(h) == expected
  }
  
  /**
   * If you insert an element into an empty heap, 
   * then delete the minimum, the resulting heap should be empty.
   */
  property("deleteMin1") = forAll { (a: Int) => 
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
   * Given any heap, you should get a sorted sequence of elements 
   * when continually finding and deleting minima. 
   * (Hint: recursion and helper functions are your friends.)
   */
  property("sorted") = forAll { (h: H) => 
    def sortedList(h: H): List[Int] = {
      if (h == empty) Nil
      else findMin(h) :: sortedList(deleteMin(h))
    }
    val l = sortedList(h)
    l == l.sorted
  }

  /**
   * Finding a minimum of the melding of any two heaps 
   * should return a minimum of one or the other.
   */
  property("melding") = forAll { (h1: H, h2: H) => (h1, h2) match {
      case h if h._1 == empty && h._2 == empty => true
      case h if h._1 == empty && h._2 != empty => findMin(meld(h1, h2)) == findMin(h2)
      case h if h._1 != empty && h._2 == empty => findMin(meld(h1, h2)) == findMin(h1)
      case _ => {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        val expected = if (min1 < min2) min1 else min2
        findMin(meld(h1, h2)) == expected
      }
    }
  }

  property("link") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a + 1 else a
    val h1 = insert(b + 1, insert(b, insert(b + 2, empty)))
    val h2 = deleteMin(h1)
    h2 == insert(b+2, insert(b+1, empty))
  }


