/*
P13 (**) Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
Example:

scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
*/

val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

//attempt with recursion
def encodeDirect[T](xs : List[T]) : List[(Int, T)] = {
  def acc (xs : List[T], result : List[(Int, T)]) : List[(Int, T)] = xs match {
    case y:: ys => {val (as, bs) = (ys.span(_ == y)); acc(bs, (as.length + 1, y) :: result)}
    case Nil => result
  }
  acc(xs, List[(Int, T)]()).reverse
}
data.takeWhile(_ == data.head)

encodeDirect(data)



 /* Official Answer
Similar, except I used an inner function, and case statements (which led to the +1 hack)

 lessons learnt
 1. when working with lists, and you don't want to deconstruct them, then using if expressions rather
  than match should be considered


// P13 (**) Run-length encoding of a list (direct solution).
//     Implement the so-called run-length encoding data compression method
//     directly.  I.e. don't use other methods you've written (like P09's
//     pack); do all the work directly.
//
//     Example:
//     scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

 object P13 {
 // This is basically a modification of P09.
  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
     val (packed, next) = ls span { _ == ls.head }
     (packed.length, packed.head) :: encodeDirect(next)
  }
}

*/



