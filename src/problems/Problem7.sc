/*
P07 (**) Flatten a nested list structure.
Example:
  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
res0: List[Any] = List(1, 1, 2, 3, 5, 8)
*/


val simplelist = List(List(1, 1), List(3, List(5, 8)))
val compoundList = List(List(1, 1), 2, List(3, List(5, 8)))

//Attempt 1: use built in methods
//only works if a list contains only containers
simplelist.flatten
//this will fail (as it has non seq members
//compoundList.flatten


//Attempt 2: try implicit conversion on input to accommodate flattens default behaviour
//this is nasty, as it gives erasure warnings
implicit def any2List[A](x: A): List[A] = x match {
  case a: List[A] => a
  case _ => x :: Nil
}

//single level works ok - implicit seems to do the trick
def flattenOnce[T, A <% List[T]](ls: List[A]): List[T] = {
  ls.flatten
}
flattenOnce(simplelist)
flattenOnce(compoundList)

def flatten[T, A <% List[T]](ls: List[A]): List[T] = {
  if (ls.isEmpty) Nil
  else {
    val once = ls.flatten
    val singles = once.filter(!_.isInstanceOf[List[T]])
    val rest = for (i <- once if i.isInstanceOf[List[T]]) yield i
    singles ::: flatten(rest)
  }
}

flatten(simplelist)
flatten(compoundList)


//the above worked, but doesnt seem all that nice of a solution - I had to use implicits, as well as instanceof...
//compiler generated a lot of type erasure warnings
//Attempt 3: try again using pure recursion to see how that would look.

def flattenR(ls: List[Any]): List[Any] = {
  def walk(ls: List[Any], result: List[Any]): List[Any] = ls match {
    case x :: xs if !x.isInstanceOf[List[Any]] => walk(xs, x :: result)
    case x :: xs if x.isInstanceOf[List[Any]] => {
      val newResult = walk(x, result)
      walk(xs, newResult)
    }
    case Nil => result.reverse
  }

  walk(ls, Nil)
}
flattenR(simplelist)
flattenR(compoundList)

//not pleased that I can't figure out a nice way to do this in a typesafe way without implicits...


//what about foldLeft - is there anything I can do with that?

def flattenF(ls: List[Any]): List[Any] =
  ls.foldLeft(List[Any]())((res, next) => if (next.isInstanceOf[List[Any]]) flattenF(next) ::: res else next :: res).reverse

flattenF(simplelist)
flattenF(compoundList)

/* Official Answer
This makes my answers all look pretty weak! I was a fair way off the mark even though they worked.
Interestingly they also had to use List[Any] - I was hoping that I'd see a notation for "either this type or that"

Lessons learnt
1. the library flatten function requires that all elements are collections
2. implicits + views are tricky and powerful, but would make the code harder for others to read
3. flatmap as a way of treating the cases in match as a single unit (and so avoiding having to use .isInstanceOf
 in pattern matching guards.

 */
// P07 (**) Flatten a nested list structure.
//     Example:
//     scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
//     res0: List[Any] = List(1, 1, 2, 3, 5, 8)

object P07 {
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }
}
