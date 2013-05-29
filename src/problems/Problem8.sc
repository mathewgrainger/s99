/*
P08 (**) Eliminate consecutive duplicates of list elements.
  If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  Example:

  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
*/

val test1 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


//attempt 1 - can I use fold to do this?
def compressF[T](ls : List[T]) =
  ls.foldRight(List[T]())((n, acc) =>
    if(acc.size == 0 || acc.head != n) n :: acc else acc)

compressF(test1)


//attempt 2 - straight recurstion
def compressR[T](ls : List[T]) = {
  def acc[T](xs : List[T], result : List[T]) : List[T] =  xs match {
    case Nil => result
    case y :: ys if result.size > 0 && y == result.head => acc(ys, result)
    case y :: ys if result.size == 0 => acc(ys, y :: result)
    case y :: ys => acc(ys, y :: result)
  }
  acc(ls, Nil).reverse
}
compressR(test1)


/*

Official Answer
My fold solution matched the official answer almost exactly
I never had any ideas along the lines of the standard recursive answer
My recursive is far less elegant that the offical one


lessons learnt

1. the order of the result list can be handled by the type of fold you do (saves doing a reverse on the results)
2. use dropWhile to skip multiples in lists, rather thank doing it manually - this would have made the
match statements a lot simpler



// P08 (**) Eliminate consecutive duplicates of list elements.
//     If a list contains repeated elements they should be replaced with a
//     single copy of the element.  The order of the elements should not be
//     changed.
//
//     Example:
//     scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
//     res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 {
  // Standard recursive.
  def compressRecursive[A](ls: List[A]): List[A] = ls match {
    case Nil       => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  // Tail recursive.
  def compressTailRecursive[A](ls: List[A]): List[A] = {
    def compressR(result: List[A], curList: List[A]): List[A] = curList match {
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
      case Nil       => result.reverse
    }
    compressR(Nil, ls)
  }

  // Functional.
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }
}

 */
