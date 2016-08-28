package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { a: Int =>
    forAll { b: Int =>
      val h = insert(a, insert(b, empty))

      if (a <= b) {
        findMin(h) == a
      }
      else {
        findMin(h) == b
      }
    }
  }
  property("min3") = forAll { a: Int =>
    val b = 1
    val h = insert(a, insert(b, empty))

    if (a <= b) {
      findMin(h) == a
    }
    else {
      findMin(h) == b
    }
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("empty1") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sort1") = forAll { (h: H) =>
    val list = heapToList(h)
    list == list.sorted
  }

  def heapToList(h: H): List[A] = {
    if (isEmpty(h)) {
      List()
    }
    else {
      val min = findMin(h)
      min :: heapToList(deleteMin(h))
    }
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

}
