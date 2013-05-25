/*
P03 (*) Find the Kth element of a list.

By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2

 */

//simple approach

def nthSimple[T](n: Int, xs: Seq[T]) = xs(n)

nthSimple(2, List(1, 1, 2, 3, 5, 8)) == 2

def nthHandMade[T](n: Int, xs: Seq[T]): T = {
  if (xs.isEmpty)
    throw new IndexOutOfBoundsException(n.toString)
  else if (n == 0) xs.head
  else nthHandMade(n - 1, xs.tail)
}
nthHandMade(2, List(1, 1, 2, 3, 5, 8)) == 2
nthHandMade(-1, List())
nthHandMade(0, List())
nthHandMade(1, List())

def nthHandMade2[T](n: Int, xs: Seq[T]): T = xs match {
  case y :: _ if (n == 0) => y
  case y :: ys if (n > 0) => nthHandMade2(n - 1, ys)
  case _ => throw new IndexOutOfBoundsException(n.toString)
}
nthHandMade2(2, List(1, 1, 2, 3, 5, 8)) == 2
nthHandMade2(-1, List())
nthHandMade2(1, List())
nthHandMade2(0, List())


/*        Official Answer
both similar in structure to mine. the handcrafted official answer does a much better
job of pattern matching using a pair.
lessons learnt
1. combining elements into a pair for pattern matching is a more elegant approach than using if/guards
2. if you arent going to reference a varaible in the rhs of a match then use underscore on the left


// P03 (*) Find the Kth element of a list.
//     By convention, the first element in the list is element 0.
//
//     Example:
//     scala> nth(2, List(1, 1, 2, 3, 5, 8))
//     res0: Int = 2

object P03 {
  // Trivial with builtins.
  def nthBuiltin[A](n: Int, ls: List[A]): A =
    if (n >= 0) ls(n)
    else throw new NoSuchElementException

  // Not that much harder without.
  def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (0, h :: _   ) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil      ) => throw new NoSuchElementException
  }
}

/*






