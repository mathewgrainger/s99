/*
P17 (*) Split a list into two parts.
The length of the first part is given. Use a Tuple for your result.
Example:
scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
*/

val data = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def split[T](n : Int, xs : List[T]) : (List[T], List[T]) = xs.splitAt(n)
split(3, data)


def split2[T](n : Int, xs : List[T]) : (List[T], List[T]) = {
  (xs take n, xs drop n)
}

split2(3, data)


def splitR[T](n : Int, xs : List[T]) : (List[T], List[T]) = {
  def acc(n : Int, xs : List[T], result : (List[T], List[T])) : (List[T], List[T]) = {
    if(xs.isEmpty)
      result
    else if (n> 0)  {
      val result2 = (xs.head :: result._1, result._2)
      acc(n - 1, xs.tail, result2)
    } else
      (result._1, xs)
  }
  val result = acc(n, xs, (Nil, Nil))
  (result._1.reverse, result._2)
}
splitR(3, data)


/* official answer

my recursive answer was far less tidy - match + tuple matching is a far more readable way of getting there
I did ok with the builtin + 'functional'

lessons learnt
1. use match + tuple for recursive matching where possible - its simpler to read.



// P17 (*) Split a list into two parts.
//     The length of the first part is given.  Use a Tuple for your result.
//
//     Example:
//     scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
//     res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17 {
  // Builtin.
  def splitBuiltin[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

  // Simple recursion.
  def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
    case (_, Nil)       => (Nil, Nil)
    case (0, list)      => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)
    }
  }

  // Tail recursive.
  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
        case (_, Nil)       => (pre.reverse, Nil)
        case (0, list)      => (pre.reverse, list)
        case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
      }
    splitR(n, ls, Nil)
  }

  // Functional (barely not "builtin").
  def splitFunctional[A](n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))
}
 */

