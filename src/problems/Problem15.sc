/*
P15 (**) Duplicate the elements of a list a given number of times.
Example:
  scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
*/

val data = List('a, 'b, 'c, 'c, 'd)

def duplicateN[T](n: Int, xs : List[T]) = xs.flatMap((x) => List.fill(n)(x))

duplicateN(3, data)


/* official answer

same as mine except uses make instead of fill, uses _ on the  mapping function, and uses infix syntax for flatmap

lessons learnt
1. use _ where possible to make the code more succinct and easier to read

// P15 (**) Duplicate the elements of a list a given number of times.
//     Example:
//     scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)

object P15 {
  def duplicateN[A](n: Int, ls: List[A]): List[A] =
    ls flatMap { List.make(n, _) }
}
 */

