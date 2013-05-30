/*
P12 (**) Decode a run-length encoded list.
  Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */

val data = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))


def repeat[T](n : Int, x : T) : List[T] = (0 until n).map(_ => x).toList

def decode[U](ys: List[(Int, U)]): List[U] =
  ys.foldRight(List[U]())((x, xs) =>  repeat(x._1, x._2) ::: xs)

decode(data)

/* Official Answer
official answer is much cleaner than mine - flatmap is more indicative of the thing that you
are doing.

Lessons learnt
1. use flatMap over folds if you can
2. List.make (wasn't aware of this function)
3. Having googled list.make - learnt that list.fill()() is now the correct way to do this

// P12 (**) Decode a run-length encoded list.
//     Given a run-length code list generated as specified in problem P10,
//     construct its uncompressed version.
//
//     Example:
//     scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
//     res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

object P12 {
  def decode[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.make(e._1, e._2) }
}

*/



