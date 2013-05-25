/**
 * Problem 1
 * Find the last element of a list.
 * Example:
 * scala> last(List(1, 1, 2, 3, 5, 8))
 * res0: Int = 8
 */
def last1[T](xs : Seq[T]) : T = xs.last

def last2[T](xs : Seq[T]) : T = xs match {
  case Nil => throw new ArrayIndexOutOfBoundsException()
  case y::Nil => y
  case y::ys => last2(ys)
}

val p1a = last1(List(1, 1, 2, 3, 5, 8)) == 8
val p1b = last1(List(7)) == 7

val p1c = last2(List(1, 1, 2, 3, 5, 8)) == 8
val p1d = last2(List(7)) == 7

/* Official Answer
object P01 {
  // There are several ways to solve this problem.  If we use builtins, it's very
  // easy.
  def lastBuiltin[A](ls: List[A]): A = ls.last

  // The standard functional approach is to recurse down the list until we hit
  // the end.  Scala's pattern matching makes this easy.
  def lastRecursive[A](ls: List[A]): A = ls match {
    case h :: Nil  => h
    case _ :: tail => lastRecursive(tail)
    case _         => throw new NoSuchElementException
  }
}

Pretty much the same as mine
*/


