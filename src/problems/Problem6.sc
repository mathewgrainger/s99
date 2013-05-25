/*
P06 (*) Find out whether a list is a palindrome.
  Example:
  scala> isPalindrome(List(1, 2, 3, 2, 1))
res0: Boolean = true
*/

def isPalindrome[T](xs : List[T]) : Boolean = xs == xs.reverse

isPalindrome(List(1, 2, 3, 2, 1))

/*Official Answer

I had the same idea as the solution that they came up with, but the more efficient
approach didn't occur to me. Not sure how I'd code it either


// P06 (*) Find out whether a list is a palindrome.
//     Example:
//     scala> isPalindrome(List(1, 2, 3, 2, 1))
//     res0: Boolean = true

object P06 {
  // In theory, we could be slightly more efficient than this.  This approach
  // traverses the list twice: once to reverse it, and once to check equality.
  // Technically, we only need to check the first half of the list for equality
  // with the first half of the reversed list.  The code to do that more
  // efficiently than this implementation is much more complicated, so we'll
  // leave things with this clear and concise implementation.
  def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse
}
*/