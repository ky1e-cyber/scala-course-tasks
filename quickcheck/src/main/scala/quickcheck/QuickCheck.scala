package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.compiletime.ops.boolean

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- Arbitrary.arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOf2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == ord.min(a, b)
  }

  property("empty1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted1") = forAll { (h: H) =>
    def listOfHeap(h: H, lst: List[A]): List[A] =
      if (isEmpty(h)) {
        lst
      } else {
        val m = findMin(h)
        val newHeap = deleteMin(h)
        listOfHeap(newHeap, m :: lst)
      }

    def isSortedDescending(lst: List[A]): Boolean =
      lst match {
        case Nil => true
        case _ :: Nil => true
        case hd :: tl => ord.lteq(hd, tl.head) && (isSortedDescending(tl))
      }
  
    isSortedDescending(listOfHeap(h, List())) 
  }

  property("minOfMeld1") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) {
      true
    } else {
      val (m1, m2) = (findMin(h1), findMin(h2))
      findMin(meld(h1, h2)) == ord.min(m1, m2)
    }
  } 

