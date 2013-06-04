/*
P20 (*) Remove the Kth element from a list.
Return the list and the removed element in a Tuple. Elements are numbered from 0.
Example:

scala> removeAt(1, List('a, 'b, 'c, 'd))
res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
*/

def removeAt[T](n : Int, xs : List[T]) =
  (xs.take(n) ::: xs.drop(n + 1), xs(n))

removeAt(1, List('a, 'b, 'c, 'd))

/*

similar answer to to the non-recursive one, but without error handling.

lessons learnt
1. splitAt would have been a better choice than take/drop
2.
// P20 (*) Remove the Kth element from a list.
//     Return the list and the removed element in a Tuple.  Elements are
//     numbered from 0.
//
//     Example:
//     scala> removeAt(1, List('a, 'b, 'c, 'd))
//     res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)

object P20 {
  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }

  // Alternate, with fewer builtins.
  def removeAt2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) => {
        val (t, e) = removeAt(n - 1, ls.tail)
        (ls.head :: t, e)
      }
    }
}
*/