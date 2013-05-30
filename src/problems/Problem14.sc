/*
P14 (*) Duplicate the elements of a list.
  Example:
  scala> duplicate(List('a, 'b, 'c, 'c, 'd))
res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
*/

val data = List('a, 'b, 'c, 'c, 'd)

def duplicate[T](xs : List[T]) = xs.flatMap((x) => List.fill(2)(x))

duplicate(data)


/* official answer

very similat to mine  - uses different fn to create the list, and a slightly different syntax when calling flatmap

// P14 (*) Duplicate the elements of a list.
//     Example:
//     scala> duplicate(List('a, 'b, 'c, 'c, 'd))
//     res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)

object P14 {
  def duplicate[A](ls: List[A]): List[A] = ls flatMap { e => List(e, e) }
}
*/