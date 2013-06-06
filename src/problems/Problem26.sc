/*
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there
are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians,
this result may be great. But we want to really generate all the possibilities.
Example:

scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

*/

val data = List(1, 3, 5)

def combinations[T](n : Int, xs : List[T]) : List[List[T]] = {

  def generate(paths : List[List[T]]) : List[List[T]] = {
    for {
      y <- xs
      ys <- paths if !ys.contains(y)
    } yield y :: ys
  }

  def acc(n : Int, paths : List[List[T]]) : List[List[T]] = {
    if(n == 0) paths
    else {
      val newPaths = generate(paths)
      acc(n - 1, newPaths)
    }

  }

  if(n == 0) List(List[T]())
  else{
    val seed = for(x <- xs) yield List(x)
    acc(n - 1, seed)
  }

}

combinations(2, List('a, 'b, 'c))



combinations(3, List('a, 'b, 'c))



combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))    .length

def combinations2[T](n : Int, xs : List[T]) : Set[Set[T]] = {

  def generate(paths : Set[Set[T]]) : Set[Set[T]] = {
    for {
      ys <- paths
      y <- xs.filter(!ys.contains(_))
    } yield  ys + y
  }

  def acc(n : Int, paths : Set[Set[T]]) : Set[Set[T]] = {
    if(n == 0) paths
    else {
      val newPaths = generate(paths)
      acc(n - 1, newPaths)
    }

  }

  if(n == 0) Set(Set[T]())
  else{
    val seed = for(x <- xs) yield Set(x)
    acc(n - 1, seed.toSet)
  }

}

combinations2(2, List('a, 'b, 'c))


combinations2(3, List('a, 'b, 'c))


combinations2(3, List('a, 'b, 'c, 'd, 'e, 'f))    .size


/*    official answer
I got nowhere near this. I ended up cheating and using sets to restrict the duplicates

lessons learnt
1. I've got a lot to learn :-)
1a. approaching breadth first search in functional programming is a bit different to
imperative - model is start with a list of empty somethings, then expand on that state vs
loop from nothing, adding paths as we go.


// P26 (**) Generate the combinations of K distinct objects chosen from the N
//          elements of a list.
//     In how many ways can a committee of 3 be chosen from a group of 12
//     people?  We all know that there are C(12,3) = 220 possibilities (C(N,K)
//     denotes the well-known binomial coefficient).  For pure mathematicians,
//     this result may be great.  But we want to really generate all the possibilities.
//
//     Example:
//     scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
//     res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...

object P26 {
  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map {sl.head :: _}
    }
}
 */






