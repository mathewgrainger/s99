/*
P16 (**) Drop every Nth element from a list.
Example:
scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */

val data = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
val data2 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j)

def drop[T](n:Int, xs : List[T]) : List[T] =
  if(xs.isEmpty) Nil else xs.take(n - 1) ::: drop(n, xs.drop(n))

drop(3, data)
drop(3, data2)

 /* official answer

mine bears no resemblence to the official answers. Its probably simpler to read,
but I suspect the performance characteristics are different because I am using list
appends.

lessons learnt
1. if using recursion with match statements that have a compound condition, take advantage
of tuples/pairs for matching
2. the functional solution using zipWithIndex (then filter every nth, then \
remap to a single value) is kinda cool.

// P16 (**) Drop every Nth element from a list.
//     Example:
//     scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

object P16 {
  // Simple recursion.
  def dropRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => Nil
      case (1, _ :: tail) => dropR(n, tail)
      case (_, h :: tail) => h :: dropR(c - 1, tail)
    }
    dropR(n, ls)
  }

  // Tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }



 // Functional.
 def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }
}

*/
