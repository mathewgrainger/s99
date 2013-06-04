/*
P23 (**) Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
Hint: Use the solution to problem P20
*/

// from P20
def removeAt[T](n : Int, xs : List[T]) =
  (xs.take(n) ::: xs.drop(n + 1), xs(n))

val data =  List('a, 'b, 'c, 'd, 'f, 'g, 'h)
removeAt((math.random * data.length).floor.toInt, data)

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


randomSelect(3, data)


/*

similar to the tail recursive solution

lessons learnt
1. should have used val (rest, e) = ... to deconstruct the pair rather than z._1 and z._2
2.


// P23 (**) Extract a given number of randomly selected elements from a list.
//     Example:
//     scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
//     res0: List[Symbol] = List('e, 'd, 'a)
//
//     Hint: Use the answer to P20.

object P23 {
  import P20.removeAt

  def randomSelect1[A](n: Int, ls: List[A]): List[A] =
    if (n <= 0) Nil
    else {
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect1(n - 1, rest)
    }

  // It can be expensive to create a new Random instance every time, so let's
  // only do it once.
  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: randomSelectR(n - 1, rest, r)
      }
    randomSelectR(n, ls, new util.Random)
  }
}

*/