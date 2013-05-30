/*
P11 (*) Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the
result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
 */

val data = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


//from prob 9 (official answer - its more compact)
def pack[A](ls: List[A]): List[List[A]] = {
  if (ls.isEmpty) List(List())
  else {
    val (packed, next) = ls span { _ == ls.head }
    if (next == Nil) List(packed)
    else packed :: pack(next)
  }
}

//from prob 10 (my answer - it'll do :-) )
def encode[T](xs : List[T]) = pack(xs).map((a) => (a.length, a.head))
def encodeModified[T](xs : List[T]) = pack(xs).map((a) => if(a.length == 1) a.head else (a.length, a.head))
encodeModified(data)


/* Official Answer
err  - lost the plot along the way here somewhere - I wrote mine in terms of pack instead of encode....

Lessons learnt
1. The typesafe version below introduces Either - which was something I was looking for earlier
to avoid using lists of Any.

// P11 (*) Modified run-length encoding.
//     Modify the result of problem P10 in such a way that if an element has no
//     duplicates it is simply copied into the result list.  Only elements with
//     duplicates are transferred as (N, E) terms.
//
//     Example:
//     scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

object P11 {
  import P10.encode
  def encodeModified[A](ls: List[A]): List[Any] =
    encode(ls) map { t => if (t._1 == 1) t._2 else t }

  // Just for fun, here's a more typesafe version.
  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
}
*/