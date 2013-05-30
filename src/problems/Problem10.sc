/*
 P10 (*) Run-length encoding of a list.
  Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:

  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/

//from prob 9 (official answer)
def pack[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span {
      _ == ls.head
    }
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}

val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def encode[T](xs: List[T]) = pack(xs).map((a) => (a.length, a.head))

encode(data)


/* Official answer

The same as mine except I didn't use the infix notation

// P10 (*) Run-length encoding of a list.
//     Use the result of problem P09 to implement the so-called run-length
//     encoding data compression method.  Consecutive duplicates of elements are
//     encoded as tuples (N, E) where N is the number of duplicates of the
//     element E.
//
//     Example:
//     scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P10 {
 import P09.pack
 def encode[A](ls: List[A]): List[(Int, A)] =
   pack(ls) map { e => (e.length, e.head) }
}
 */


