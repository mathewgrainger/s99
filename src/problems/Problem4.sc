import scala.annotation.tailrec

/*
P04 (*) Find the number of elements of a list.
  Example:
  scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
 */

//simple
def length[T](xs : List[T]) = xs.size

length(List(1, 1, 2, 3, 5, 8)) == 6

//handmade
def length2[T](xs : List[T]) : Int = xs match {
  case Nil => 0
  case _ :: xs => 1 + length2(xs)
}

length2(List(1, 1, 2, 3, 5, 8)) == 6
length2(List()) == 0

/*Official Answer

My answers pretty closely mirrored the first 2 options presented
The tail recursive option didn't occur to me but should have.
Using foldl would never have occurred to me as an option

lessons learnt
1. when using recursion, think about tailcalls
2. scala supports tail recursion on local or final functions
3. before attempting recusion at all, think whether a fold, map or reduce is a better option



// P04 (*) Find the number of elements of a list.
//     Example:
//     scala> length(List(1, 1, 2, 3, 5, 8))
//     res0: Int = 6

object P04 {
  // Builtins.
  def lengthBuiltin[A](ls: List[A]): Int = ls.length

  // Simple recursive solution.
  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case Nil       => 0
    case _ :: tail => 1 + lengthRecursive(tail)
  }

  // Tail recursive solution.  Theoretically more efficient; with tail-call
  // elimination in the compiler, this would run in constant space.
  // Unfortunately, the JVM doesn't do tail-call elimination in the general
  // case.  Scala *will* do it if the method is either final or is a local
  // function.  In this case, `lengthR` is a local function, so it should
  // be properly optimized.
  // For more information, see
  // http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
  def lengthTailRecursive[A](ls: List[A]): Int = {
    def lengthR(result: Int, curList: List[A]): Int = curList match {
      case Nil       => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, ls)
  }

  // More pure functional solution, with folds.
  def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }
}
 */
