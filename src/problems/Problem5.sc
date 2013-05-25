/*
P05 (*) Reverse a list.
Example:
  scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/

def reverseLib[T](xs : List[T]) : List[T] = xs.reverse

List(8, 5, 3, 2, 1, 1) == reverseLib(List(1, 1, 2, 3, 5, 8))

def reverseRecursive[T](xs : List[T]) : List[T] = {
  def reverseAcc(in : List[T], acc : List[T]) : List[T] = in match {
    case z::zs => reverseAcc(zs, z :: acc)
    case Nil => acc
  }
  reverseAcc(xs, Nil)
}
List(8, 5, 3, 2, 1, 1) == reverseRecursive(List(1, 1, 2, 3, 5, 8))

def reverseFold[T](xs : List[T]) : List[T]
  = xs.foldLeft(List[T]())((ys, y) => y :: ys)

List(8, 5, 3, 2, 1, 1) == reverseFold(List(1, 1, 2, 3, 5, 8))




/* Official Answer

Got most of these - started with the simple recursive in mind, but ended up with the
tail recursive version.

Lessons Learnt
--------------
1. important to specify a type in the z value for the fold. I initially tried Nil, and received type errors, as the
 compiler needs a value with a Type, so that it can derive the types in the function applied in the fold


// P05 (*) Reverse a list.
//     Example:
//     scala> reverse(List(1, 1, 2, 3, 5, 8))
//     res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 {
  // Builtin.
  def reverseBuiltin[A](ls: List[A]): List[A] = ls.reverse

  // Simple recursive.  O(n^2)
  def reverseRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => reverseRecursive(tail) ::: List(h)
  }

  // Tail recursive.
  def reverseTailRecursive[A](ls: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil       => result
      case h :: tail => reverseR(h :: result, tail)
    }
    reverseR(Nil, ls)
  }

  // Pure functional
  def reverseFunctional[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]()) { (r, h) => h :: r }
}
 */

