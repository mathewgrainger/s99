/*
P09 (**) Pack consecutive duplicates of list elements into sublists.
  If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 */

val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

//Attempt 1 - can I use fold?

def checkIfDupAndAppend[T](n: T, ys: List[List[T]]): List[List[T]] =
  if (ys.size == 0)
    List(List(n))
  else if (n == ys.head.head)
    (n :: ys.head) :: ys.tail
  else
    (n :: Nil) :: ys


checkIfDupAndAppend('a', Nil)

checkIfDupAndAppend('a', List(List('a', 'a')))

checkIfDupAndAppend('b', List(List('a', 'a')))


def packF[T](xs: List[T])= xs.foldRight(List[List[T]]())(checkIfDupAndAppend)

packF(data)

//Attempt 2 - recursion

def packR[T](xs : List[T]) = {
  def acc(ys : List[T], result : List[List[T]]) : List[List[T]] = ys match {
    case z:: zs => acc(checkIfDupAndAppend(z, result))
    case Nil => result
  }
}






packR(data)



/*

Official Answer

My answers were far more complicated. The official answer, making use of span and tuples ids much more elegant


//lessons learnt
1. didn't know that scala supported synbols
2. be careful of the type when you are setting the unit value in a fold
3. break out inner methods early - makes it easier to test/debug them. Then potentially move them back into a smaller
scope
4. use span to reduce a list a chunk at a time.

 // P09 (**) Pack consecutive duplicates of list elements into sublists.
//     If a list contains repeated elements they should be placed in separate
//     sublists.
//
//     Example:
//     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 {
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}


*/


