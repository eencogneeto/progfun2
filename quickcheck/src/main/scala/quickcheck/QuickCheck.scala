package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      n <- oneOf(const(empty), genHeap)
    } yield insert(x, n)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("1. insert any two elements into an empty heap, finding minimum should return smaller number") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == (if (a < b) a else b)
  }

  property("2. delete from single element heap") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  def heapToList(h: H): List[A] =
    if isEmpty(h) then List() else (findMin(h) :: heapToList(deleteMin(h)))

  def listToHeap(l: List[Int], h: H): H = l match
    case Nil => h
    case x :: xs => listToHeap(xs, insert(x, h))

  property("3. sorted sequence of elements when continually finding and deleting minima") = forAll { (l: List[Int]) =>
    val result = heapToList(listToHeap(l, empty))
    val l_sorted = l.sorted
    result == l_sorted
  }

  property("4. finding a minimum of the melding of any two heaps should return the minimum of one or the other") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    (isEmpty(h1), isEmpty(h2)) match
      case (true, true) => isEmpty(m)
      case (false, true) => findMin(m) == findMin(h1)
      case (true, false) => findMin(m) == findMin(h2)
      case _ => findMin(m) == (findMin(h1) min findMin(h2))
  }

