/*
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
*/

def lotto(size : Int, range : Int) = for(i <- 0 until size) yield math.random.*(range).ceil.toInt

lotto(6, 49)

/*
official answer
quite different to mine - I didn't reuse the existing functions, and so didn't need a range

// P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//     Example:
//     scala> lotto(6, 49)
//     res0: List[Int] = List(23, 1, 17, 33, 21, 37)

object P24 {
  import P23.randomSelect
  def lotto(count: Int, max: Int): List[Int] =
    randomSelect(count, List.range(1, max + 1))
}
/*