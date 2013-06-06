/*
P25 (*) Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.
Example:

scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
*/

//reuse code from earlier problems
def removeAt[T](n : Int, xs : List[T]) =
  (xs.take(n) ::: xs.drop(n + 1), xs(n))

def randomSelect[T](n : Int, xs : List[T]) = {
  def worker(n : Int, xs : List[T], result : List[T]) : List[T] = (n, xs) match {
    case (0, _) => result
    case (_, Nil) => result
    case _ => {
      val z = removeAt((math.random * xs.length).floor.toInt, xs)
      worker(n - 1, z._1, z._2 :: result)
    }
  }
  worker(n, xs, Nil)
}
 // new code

def randomPermute[T](xs : List[T]) = randomSelect(xs.length, xs)

randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))

/*
official answer

got the simple but inefficient version

// P25 (*) Generate a random permutation of the elements of a list.
//     Hint: Use the solution of problem P23.
//
//     Example:
//     scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)

object P25 {
  // This algorithm is O(n^2), but it makes up for that in simplicity of
  // implementation.
  import P23.randomSelect
  def randomPermute1[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)

  // The canonical way to shuffle imperatively is Fisher-Yates.  It requires a
  // mutable array.  This is O(n).
  def randomPermute[A](ls: List[A]): List[A] = {
    val rand = new util.Random
    val a = ls.toArray
    for (i <- a.length - 1 to 1 by -1) {
      val i1 = rand.nextInt(i + 1)
      val t = a(i)
      a.update(i, a(i1))
      a.update(i1, t)
    }
    a.toList
  }

  // Efficient purely functional algorithms for shuffling are a lot harder.  One
  // is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
  // Haskell. Implementing it in Scala is left as an exercise for the reader.
}

*/