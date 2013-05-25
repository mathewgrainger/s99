package sidetrips.trampolines

import scala.Predef._
import scala.annotation.tailrec

/*
Interesting article I came across due to problem 4.
http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html

I'd read about trampolines before when playing with clojure, but I don't think the concept had
fully sunk in. The article above does a nice job of explaining it.
*/

//this one is a reasonable first attempt(based heavily on the article), but I didn't manage to get
//the thunks to evaluate lazily until I added a results method that forces tailcall eval on the arranged
//thunks? I had to look into the standard lib at the  scala.util.control.TailCalls._ package to get
//even close.
object TrampolinesHandmade extends App {
  println(even2(0).result)
  println(odd2(1).result)
  println(odd2(0).result)
  println(even2(10).result)
  println(even2(11).result)
  println(even2(20000).result)

  sealed trait Bounce[+T] {
    def result: T = {
      @tailrec
      def doResult(body : Bounce[T]) : T = body match {
        case Done(value) => value
        case Call(thunk) => doResult(thunk())
      }
      doResult(this)
    }
  }

  case class Done[T](override val result : T) extends Bounce[T]
  case class Call[T](thunk: () => Bounce[T]) extends Bounce[T]

  def trampoline[T](b: Bounce[T]): T = b match {
    case Done(result) => result
    case Call(thunk) => trampoline(thunk())
  }

  def even2(n: Int): Bounce[Boolean] = {
    if (n == 0) Done(true)
    else Call(() => odd2(n - 1))
  }

  def odd2(n: Int): Bounce[Boolean] = {
    if (n == 0) Done(false)
    else Call(() => even2(n - 1))
  }
}


/**
 * So, lets now do things the easier way using the scala libs.
 */

import scala.util.control.TailCalls._

object TrampolinesStandard extends App {

  def even3(n: Int): TailRec[Boolean] =
    if (n == 0) done(true) else tailcall(odd3(n - 1))


  def odd3(n : Int): TailRec[Boolean] =
    if (n == 0) done(false) else tailcall(even3(n - 1))


  println(even3(0).result)
  println(odd3(1).result)
  println(odd3(0).result)
  println(even3(10).result)
  println(even3(11).result)
  println(even3(20000).result)

}
