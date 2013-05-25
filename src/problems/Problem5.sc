/*
P05 (*) Reverse a list.
Example:
  scala> reverse(List(1, 1, 2, 3, 5, 8))
res0: List[Int] = List(8, 5, 3, 2, 1, 1)
*/

def reverseLib[T](xs : List[T]) : List[T] = xs.reverse

List(8, 5, 3, 2, 1, 1) == reverseLib(List(1, 1, 2, 3, 5, 8))

//todo : up to here - implement other approaches...
