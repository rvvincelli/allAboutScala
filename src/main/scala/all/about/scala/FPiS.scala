package all.about.scala

import annotation.tailrec
import collection.immutable.{Stream => SStream, Map => SMap}
import concurrent.{Await, Future}
import concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import java.io.FileWriter
import java.util.concurrent.{Executors, Callable, ExecutorService, Future => JavaFuture, FutureTask => JavaFutureTask}
import math._
import math.Ordering.OptionOrdering
import util.{Random, Try}
import util.matching.Regex

object FPiS {
  
  //Programming with side effects:
  trait CreditCard {
    def charge(price: Int) //talk to an external service...
  }
  case class Coffee(price: Int = 1)
  class Cafe {
    //Not really reusable: say we wanna buy 20 coffees, then we need to call this 20 times! Overheads!
    //NO1
    def buyCoffee(cc: CreditCard): Coffee = {
	    val cup = new Coffee()
	    cc.charge(cup.price) //side effect!
	    cup 
	  }
  }
  
  //Testability improvement: Payment could be an interface and tested out alone
  trait Payments {
    def charge(price: Int)
  }
  class ImprCafe {
    def imprBuyCoffee(pay: Payments): Coffee = {
	    val cup = new Coffee()
	    pay.charge(cup.price)
	    cup
    }
  }

  //Towards a functional solution. The first major separation of concerns is making a distinction between creating and
  //returning the amount to be charged (here) versus interpreting the side effects (charging, above) immediately inside
  //the function body.
   case class Charge(cc: CreditCard, amount: Double) {
    def combine(other: Charge): Charge = //this method can be used to send multiple payments on a given credit card
      if (cc == other.cc)
      Charge(cc, amount + other.amount)
      else
        throw new Exception("Can't combine charges to different cards")
  }
  
  class CafeFun {
	  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
	    val cup = new Coffee()
	    (cup, Charge(cc, cup.price))
	  }
	  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
	  }
  }
  //Now these two methods are easily testable and Cafe is ignorant of how the payment charge is processed.
 
  //FP: (almost) everything is a (pure) function. A pure function is one that lacks side effects. So a Function[A, B]
  //takes an arbitrary value of type A and returns one and only one value of type B; for an input "a" the output "b"
  //is determined only by the "a" itself, ie no state whatsoever is relevant in this sense. Lacking side effects
  //means that no event other than the return can be observed. The notion of a function is what we define in math,
  //when programming, depending on the language, names come and go. In Scala Function[A, B] is the reification of a
  //method, which can have or not have side effects; if it has and it takes no params we write foo(). The word
  //"procedure" can be used to mean a method with side effects. A side effect could be calling an external function,
  //or modifying one of the inputs.
  
  //Referential transparency. An expression in a programming language is a sequence of entities (values, operators...)
  //which can be realized into a value; from this it follows that whatever line "compiles" can be seen as an expression.
  //An expression has the property of RT if, taken a program where it occurs and another program defined from it by
  //substituting the value with its realization show the same behavior. For example we can always inline an expression
  //like 2+3 with 5. A function can be said pure if whenever its arguments have the RT property it has the property too.
  
  //Method buyCoffee NO1 doesn't have the RT property: let P be an arbitrary program and consider the method as an
  //expression; the expression is buyCoffee(x) where x is an instance of CreditCard and its realization value is an
  //instance of Coffee. By placing buyCoffee(x) into P, together with the effects of P if any, we can observe an
  //interaction with the payment system of card x because of the side-effecting line "cc.charge(cup.price)", whereas
  //if we place newCoffee() this does not happen.
  
  //When all of the expressions of a program are RT then it can be reasoned about as an equation or so and we say that
  //the substitution model applies: at every step a single expression can be realized until we reduce to a single one
  //yielding the final value.
  
  //FP goes hand in hand with modular programming, since it makes really easy to build complicated programs where the
  //building blocks are rather simple functions glued (composed) together.
  
  //Higher order function: a function accepting other functions as arguments (and possibly returning some again).
  
  //EX1: Write a tail recursive function to get the nth Fibonacci. Dynamic recursion, supacool!
  def fib(n: Int): Int = {
    
    @tailrec
    def go(n: Int, fibs: List[Int]): Int =
      if (n == 1) fibs.last else
      go(n-1, fibs :+ fibs.takeRight(2).sum)
    
    if (n == 0 || n == 1) n else
    go(n, List(0, 1))
  
  }
  
  //A very lame HOF:
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }
  formatResult("factorial", 42, fib)
  
  formatResult("halving", 31337, (n: Int) => n / 2) //anonymous f, which is syntactic sugar for:
  formatResult("halving", 31337, new Function[Int, Int] {def apply(n: Int) = n / 2})
  
  //A still lame yet polymorphic one:
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
	  @annotation.tailrec
	  def loop(n: Int): Int =
	    if (n >= as.length) -1
	    else if (p(as(n))) n
	    else loop(n + 1)
	  loop(0) 
	}
  
  //EX2 Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function (lose
  //the given, more fun)
  def isSorted[A](as: Array[A])(implicit ord: Ordering[A]): Boolean =  
    if (as.length == 0 || as.length == 1) true else
    if (as.length == 2) ord.lteq(as(0), as(1)) else
    if (ord.lteq(as(0), as(1))) isSorted(as.tail) else false
    
  //EX3: 3-curried; since the => operator is right associative A => (B => C) is equivalent to A => B => C
  def curry3[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  //If you can exchange an apple and a banana for a carrot, you can exchange an apple for the right to exchange a 
  //banana for a carrot. Simply mindblowing.
  
  //EX4: 3-uncurried
  def uncurry3[A, B, C](f: A => (B => C)): (A, B) => C = (a: A, b: B) => f(a)(b)
  
  //EX5: function composition
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
  
  //All of these methods have a single immediate implementation following from the signature
  
  //EX6: implement List.tail
  def tail[A](list: List[A]) = list match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case _ :: xs => xs
  }
  
  //EX6: setHead
  def setHead[A](list: List[A], newHead: A) = list match {
    case Nil => throw new UnsupportedOperationException("no head to replace")
    case _ :: xs => newHead :: xs
  }
  
  //EX7: drop the first n elements
  @tailrec
  def drop[A](list: List[A], n: Int): List[A] = {
    assert(n >= 0)
    if (n == 0) list else
    if (n >= list.length) throw new UnsupportedOperationException(s"cannot drop $n elements in a list of length ${list.length}") else
    drop(list.tail, n-1)
  }
  
  //EX8: dropWhile, drops elements from the list as long as they match the predicate
  @tailrec
  def dropWhile[A](list: List[A], p: A => Boolean): List[A] = 
    if (list.isEmpty || !p(list.head)) list else
    dropWhile(list.tail, p)
  
  //Predicate type is of course inferred if we hint the compiler, for example:
  dropWhile[Int](Nil, _ => true)
  dropWhile[Int](Nil, x => x<0)
  //but if not:
  //dropWhile(List(1,2), _ => true) does not compile
  //dropWhile(List(1,2), x => x<0) does not either
  //We need to add :Int or curry it:
  (dropWhile[Int]_).curried(List(1, 2))(_ => true)
  (dropWhile[Int]_).curried(List(1, 2))(x => x<0); (dropWhile[Int]_).curried(List(1, 2))(_<0) 
  //We already encountered this and that's a limitation of the Scala compiler, Haskell does not have it for example
  
  //EX9: init, returns all of the elements except the last one
  def init[A](list: List[A]) = {
	  
    @tailrec
    def go(list: List[A], initList: List[A]): List[A] = 
	    if (list.length == 1) initList
	    else go(list.tail, initList.:+(list.head))
    
    if (list.isEmpty) throw new UnsupportedOperationException(s"cannot drop the last element in an empty list") else
    if (list.length == 1) Nil else
    go(list, Nil)
    
  }
  //EX10: can product, implemented using foldRight, immediately halt the recursion and return 0 if it encounters a 0?
  def product[A](list: List[A])(implicit num: Numeric[A]) = list.:\(num.one)(num.times(_, _)) // :\ - rightleft domino!
  //Remember: right fold is right associative, the computation proceeds from right to left: 1 + (2 + (3 + (4 + 5)))
  //It seems we cannot since if we take foldRight as a black box then we don't have control on the way it executes.
  //We can of course have an explicit function immediately returning 0 if the head of the call is 0
  
  //scan returns the whole list, not just the final value:
  assert(List(1, 2, 3, 4).scan(0)(_+_) == List(0, 1, 3, 6, 10)) 
  
  //EX11: compute the length of a list using foldRight
  def length(list: List[Int]) = list.:\(0)((_, y)=>1+y)
  
  //EX12: compute the sum of a list using foldLeft
  def sum(list: List[Int]) = list./:(0)(_+_)
  
  //EX13: compute the reverse of a list using a fold
  def reverse[A](list: List[A]) = list./:(List.empty[A])((acc, x) => acc.+:(x))
  
  //EX14: foldLeft in terms of foldRight
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = l.foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(z)
  //From http://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala 
  //We can view the process as follows. For each element a of l we take the partial application b => f(b,a), which is 
  //a function B => B. Then, we compose all these functions in such a way that the function corresponding to the 
  //rightmost element (with which we start the traversal) is at far left in the composition chain. Finally, we apply
  //the big composed function on z. This results in a sequence of operations that starts with the leftmost element 
  //(which is at far right in the composition chain) and finishes with the right most one.
  //An equivalent definition:
  def foldLeftViaFoldRight2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
	  def h(a: A, g: B => B): (B => B) = g compose ((x: B) => f(x, a))
	  l.foldRight(identity[B] _)(h _)(z)
  }
  
  //EX15: append in terms of fold
  def append[A](list: List[A], x: A) = list.foldRight(List(x))(_::_)
  //If we want to use the append :: method then we are forced to a right fold since it is right-associative! In Scala
  //any method ending with : is right-associative.
  
  //EX16: a function that concatenates multiple lists into one, linear in the number of lists
  def concat[A](list: List[A]*) = list.foldRight(List.empty[A])(_:::_)
  
  //EX17: add 1 to each element
  def addOne[A](list: List[A])(implicit num: Numeric[A]) = list map { num.plus(_, num.one) }
  
  //EX18: from Double to String
  def doubleToString(list: List[Double]) = list map { _.toString }
  
  //EX19: redefine map
  def map[A, B](l: List[A])(f: A => B): List[B] = {
  
    @tailrec
    def go(l: List[A], acc: List[B]): List[B] = 
	    if (l.isEmpty) acc else
	    go(l.tail, acc.:+(f(l.head)))
    
    go(l, List.empty[B])
  
  }
  
  //EX20: filter
  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    
    @tailrec
    def go(l: List[A], acc: List[A]): List[A] = 
	    if (l.isEmpty) acc else
	    go(l.tail, if (p(l.head)) acc.:+(l.head) else acc)
    
    go(l, List.empty[A])
  
  }
  
  //EX21: flatMap
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
  
    @tailrec
    def go(l: List[A], acc: List[B]): List[B] = 
	    if (l.isEmpty) acc else
	    go(l.tail, acc:::(f(l.head)))
    
    go(l, List.empty[B])
  
  }
  //Just like filter but with list concat rather than list element appension
  
  //EX22: filter in terms of flatMap
  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)
  
  //EX23: zipfun
  def zipfun[A](l1: List[A], l2: List[A])(f: (A, A) => A)(implicit num: Numeric[A]) = (l1 zip l2) map { case (x, y) => f(x, y) }
  
  //EX24: sublist
  def isSublist(small: List[_], big: List[_]) = {
    
    @tailrec
    def go(s: List[_], b: List[_], from: Int): Boolean =
      if (s.isEmpty) true else 
      if (b.isEmpty) false else
      if (s.head == b.head) go(s.tail, b.tail, from) else
      go(small, big.takeRight(from), from-1)
      
    go(small, big, big.length-1)
    
  }
  //Returns true if the list small is exactly contained in the list big: therefore {1,2} is a sublist of {1,2,3}
  //but not of {1,4,56,2}
  
  //EX25: subsequence
  def isSubsequence(small: List[_], big: List[_]) = {
    
    @tailrec
    def go(s: List[_], b: List[_]): Boolean =
      if (s.isEmpty) true else 
      if (b.isEmpty) false else
      if (s.head == b.head) go(s.tail, b.tail) else
      go(s, b.tail)
      
    go(small, big)
    
  }
  //Returns true if scanning from the head in the big list i can stack the elements of the small: both the calls in
  //isSublist above are true then

  //Algebraic data type (ADT): a data type defined by one or more data constructors, each possibly accepting arguments,
  //composable by means of operations. Each data constructor is said to be the product of its arguments and the sum
  //of the constructors yields the data type. For example a list is defined with:
  //nil
  //the empty list, as well as:
  //list(elem, list)
  //which defines a list recursively as a structure having a single value, the head, followed by another, possibly
  //empty, list. 
  //The terms "sum" and "product" are used since structures like this most of the time are amenable as algebraic
  //structures such as groups. Take for example the set of all the lists on a domain it is easy to see that it supports
  //the following group axioms:
  //-the empty list is the zero: concatenating it to a list yields the same list
  //-concatenating any two lists still gives a member of this set
  //-associativity, concatenation is associative:
  // (A+B)+C=A+(B+C) => {a1,...,am,b1,...,bn}+{c1,...,co}={a1,...,am}+{b1,...,bn,c1,...co} => 
  // {a1,...,am,b1,...,bn,c1,...co}={a1,...,am,b1,...,bn,c1,...co}
  //There exists no inverse element though, so we can't say this' a group, it is a monoid.
  
  //A binary tree is another very famous example:
  sealed trait Tree[+A]
  case object EmptyTree extends Tree[Nothing]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  
  //EX26: count the number of nodes
	def count(tree: Tree[_]) = {  
  
    def go(tree: Tree[_]): Int = tree match {
      case EmptyTree => 0
      case Node(_, left, right) => 1 + go(left) + go(right)
    }
  
    go(tree)
  
  }
  
  //EX27: find the maximum element
  def max[A](tree: Tree[A])(implicit ord: OptionOrdering[A]) = {  

    def go(tree: Tree[A]): Option[A] = tree match {
      case EmptyTree => None
      case Node(value, left, right) => ord.max(Some(value), ord.max(go(left), go(right)))
    }
  
    go(tree)
  
  }
  
  //EX28: depth; by depth we mean the number of values we find along the longest root-leaf path, therefore the empty
  //tree has depth zero, the singleton tree depth one and so on
  def depth(tree: Tree[_]) = {  
    
    def go(tree: Tree[_]): Int = tree match {
      case EmptyTree => 0
      case Node(_, left, right) => 1 + (go(left) max go(right))
    }
  
    go(tree)
  
  }
  
  //EX29: map
  def map[A, B](tree: Tree[A])(f: A => B) = {  
    
    def go(tree: Tree[A]): Tree[B] = tree match {
      case et@EmptyTree => et
      case Node(value, left, right) => Node(f(value), go(left), go(right))
    }
  
    go(tree)
  
  }
  
  //EX30: a folding operation to abstract from these operations
  def fold[A, B](tree: Tree[A])(b: B, f: A => (B, B) => B) = {
    
    def go(tree: Tree[A]): B = tree match {
      case EmptyTree => b
      case Node(value, left, right) => f(value)(go(left), go(right))
    }
    
    go(tree)
  
  }
  
  //Test with:
  val tree = 
	  Node(8, 
	    Node(5, 
	      Node(9, 
	        EmptyTree, 
	        EmptyTree
	      ), 
	      Node(7, 
	        Node(1, 
	          EmptyTree, 
	          EmptyTree
	        ), 
	        Node(12, 
	          Node(2, 
	            EmptyTree, 
	            EmptyTree
	          ), 
	          EmptyTree
	        )
	      )
	    ), 
	    Node(4, 
	      EmptyTree, 
	      Node(11, 
	        Node(3, 
	          EmptyTree, 
	          EmptyTree
	        ),
	        EmptyTree
	      )
	    )
	  )
  
  //A map for example:
  val g: Int => Int = (a: Int) => 2*a
  val f: Int => (Tree[Int], Tree[Int]) => Tree[Int] = (a: Int) => (left: Tree[Int], right: Tree[Int]) => Node(g(a), left, right)
  fold(tree)(EmptyTree, f)

  
  //Throwing an exception is a side effect, and we don't want side effects while still making good use of the
  //consolidation and centralization of error-handling object that comes with the utilities for handling exceptions 
  //in Java. An exception throw breaks referential transparency, for example:
  def failingFn(i: Int): Int = {
	  val y: Int = ((throw new Exception("fail!")) :Int) //Int :> Nothing
	  try { val x = 42 + 5; x + y }
	  catch { case e: Exception => 43 }
  }
  //Now fix x and compare this program with a modified version where we substitute ((throw... for y in x+y: RT is broken
  //because the second is an invalid program (it does not compile).
  
  //With exception handling in general we are context dependent. In the example, this context is how we are going to
  //handle the exception in case it occurs, the catch block. Most importantly in Scala there are no checked exceptions,
  //therefore the compiler cannot force the caller to decide how to handle an exception thrown by the callee. The
  //@throws annotation in Scala exists to support a Java client.
  
  //Idea: to throw an "exceptional value" rather than a pure exception, much like an error code, but typesafe, so
  //that the caller is completely aware of what to expect from the called method. 
  //Say we have the following:
  def mean[A : Numeric](list: List[A]): A = 
    if (list.isEmpty) throw new UnsupportedOperationException("cannot compute the mean value of an empty list")
    else implicitly[Numeric[A]] match {
      case num: Fractional[_] => import num._; list.sum/fromInt(list.length)
      case num: Integral[_] => import num._; list.sum/fromInt(list.length)
      case _ => throw new UnsupportedOperationException("cannot compute the mean value of an undivisible numeric type")
    }
  //Now this mean function on lists is quite generic and can throw two different exceptions. For the first we could
  //have instead:
  def mean2[A : Numeric](list: List[A]): A = implicitly[Numeric[A]] match {
    case num: Fractional[_] => import num._; list.sum/fromInt(list.length)
    case num: Integral[_] => import num._; list.sum/fromInt(list.length)
    case _ => throw new UnsupportedOperationException("cannot compute the mean value of an undivisible numeric type")
  }
  //and therefore return NaN if the list is empty. Again, we could return a sentinel value, but this is very weak:
  def mean3[A : Numeric](list: List[A]): A = implicitly[Numeric[A]] match {
    case num if list isEmpty => num.zero
    case num: Fractional[_] => import num._; list.sum/fromInt(list.length)
    case num: Integral[_] => import num._; list.sum/fromInt(list.length)
    case _ => throw new UnsupportedOperationException("cannot compute the mean value of an undivisible numeric type")
  }
  //Blasphemous, because:
  //-errors may propagated if there is no check at caller site
  //-if there is check then it comes with boilerplate
  //-no applicable to polymorphic code if no sentinel value (eg min, max, impossible) even exists
  //This results in far reduced amenity when working with such functions in a higher-order context.
  
  //Caller-aided:
  def mean4[A : Numeric](list: List[A], onEmpty: A): A = 
    if (list.isEmpty) onEmpty
    else implicitly[Numeric[A]] match {
      case num: Fractional[_] => import num._; list.sum/fromInt(list.length)
      case num: Integral[_] => import num._; list.sum/fromInt(list.length)
      case _ => throw new UnsupportedOperationException("cannot compute the mean value of an undivisible numeric type")
    }
  //Limit: we are forced to return a value of the same type and we are supposed to know what in advance.
  
  //We start to be reasonable if we decide to optionally return a value: 
  def mean5[A : Numeric](list: List[A]): Option[A] = 
    if (list.isEmpty) None 
    else implicitly[Numeric[A]] match {
      case num: Fractional[_] => import num._; Some(list.sum/fromInt(list.length))
      case num: Integral[_] => import num._; Some(list.sum/fromInt(list.length))
      case _ => throw new UnsupportedOperationException("cannot compute the mean value of an undivisible numeric type")
    }

  //EX30 Implement the classic monad-like functions on Option. We are allowed to use pattern matching only for map
  //and getOrElse!
  sealed trait Option[+A] {
	  def map[B](f: A => B): Option[B] = this match {
	    case Some(a) => Some(f(a))
	    case n@None => n
	  }
	  def getOrElse[B >: A](default: => B): B = this match {
	    case Some(a) => a
	    case None => default
	  }
	  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None) 
	  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(_ => this).getOrElse(ob)
	  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) this else None)
	  //EX31 maptwee
    def map2[B, C](that: Option[B])(f: (A, B) => C): Option[C] = for {
	    a <- this
	    b <- that
	  } yield f(a, b)
	  //option pattern matching can be tedious, for to the rescue!
  }
  object Option {
    def apply[A](a: A): Option[A] = Some(a)
    def empty[A]: Option[A] = None
    implicit def fromScalaOption[A](scalopt: scala.Option[A]): Option[A] = scalopt match {
      case scala.None => None
      case scala.Some(s) => Some(s)
    }
    implicit def toScalaOption[A](opt: Option[A]): scala.Option[A] = opt match {
      case None => scala.None
      case Some(s) => scala.Some(s)
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  
  //x: => X above is a call-by-name argument as we know, this is basically a lazy evaluation by which the argument is 
  //evaluated every time it is used rather than being evaluated and assigned once at call time. A function with one
  //or more call-by-name's is called non strict. Short-circuit boolean operators can be seen as non-strict functions.
  //A function can of course be strict in some parameters, non strict in others. See more below.

  //EX32 flatMap variance
  def variance(xs: List[Double]): Option[Double] = Try(mean(xs)).map(m => 
    mean(xs.flatMap(x => List(math.pow(x - m, 2))))
  ).toOption
    
  //The proliferation of option might seem to pollute the code. If we write a function which is already Option-aware
  //that's cool, otherwise we don't have to port them one by, we can just use something like:
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  def h(x: Int): Int = x
  val lifth = lift[Int, Int](h)
  lifth(Some(1))
  //we can of course lift on the domain or codomain only
  def doLift[A, B](f: A => B, default: B): Option[A] => B = _ map f getOrElse(default)
  def coLift[A, B](f: A => B): A => Option[B] = a => Option.apply(f(a))

  //EX33 the real traverse
  def optionTraverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = list.foldRight(Option(List[B]())) {
    (a, fbs) => f(a).map2(fbs)(_ :: _)
  }
  //the real real traverse comes in a good shape from scalaz, more on this later, but the real difference is that we
  //parametrize on the functor too there, not only Option
  //What is even more handy is something like:
  //EX34 - sequence
  def optionSequence[A](list: List[Option[A]]): Option[List[A]] = optionTraverse(list)(identity)
  
  //EX34 The Either type; we want the main monadic operations, right biased. There is no requirement to avoid pattern
  //matching, but there is some difference with the Option above, things are easier there since the "left" in Option
  //is None and it's an object, whereas here Left has a value and therefore it needs an argument. For example if you
  //look at the flatMap above we have a getOrElse(None), which should be a getOrElse(Left(e)), and in order to get a
  //ref to the current e we need to pattern match or asof. There could be some other cleverly playful way too (aka
  //scalhacks).
  //Note that since the operations are right-biased we don't care about the error type and we require it to be simply
  //unchanged.
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case l@Left(_) => l
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case Right(a) => a
      case Left(_) => default
    }
    //We have EE to respect the variance on E, Either[+E, +A]. This is pro forma and not importantly necessary.
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case l@Left(_) => l
    }
    def orElse[EE >: E, B >: A](ob: => Either[EE, B]): Either[EE, B] = this.map(_ => this).getOrElse(ob) //exactly as above!
    def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
	    a <- this
	    b <- that
	  } yield f(a, b) //here too!!
	  def map2Multi[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this, that) match {
	    case (Left(l1), Left(l2)) => Either.leftApply[List[EE], C](List(l1, l2))
	    case (Right(r1), Right(r2)) => Either(f(r1, r2))
	    case (_, Left(l2)) => Either.leftApply[List[EE], C](List(l2))
	    case (Left(l1), _) => Either.leftApply[List[EE], C](List(l1)) 
	  } //this could be done without pattern matching of course too
  }
  object Either {
    def apply[E, A](a: A): Either[E, A] = Right(a)
    def leftApply[E, A](e: E): Either[E, A] = Left(e)
    def eitherMonad[E] = new Monad[({type T[A] = Either[E, A]})#T] {
      override def unit[A](a: => A): Either[E, A] = apply(a)
      override def flatMap[A, B](e: Either[E, A])(f: A => Either[E, B]): Either[E, B] = e match {
	      case Right(a) => f(a)
	      case l@Left(_) => l
      }
    }
  }
  
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
  //We can see Either as an extended option, where the negative case brings a value too, typically an Exception
  
  //EX 35
  //We can see the pattern again: foldRight from a value of the monad type, Either here, and map2 on the desired
  //binary operator for the argument, here list concatenation
  def eitherTraverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] = list.foldRight(Either[E, List[B]](List[B]())) {
    (a, fbs) => f(a).map2(fbs)(_ :: _)
  } 
  def eitherSequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = eitherTraverse(list)(identity)
  
  //EX 36 - A map2 keeping both errors: see in the Either map2Multi above.
  
  //Scala does not cache call-by-name arguments: they are evaluated everytime they are referenced in the body. We can
  //use the lazy keyword to require the value to be cached for subsequent references.
  //Definition of a stream, a lazy list:
  sealed trait Stream[+A] {
    
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h()) //h() forces evaluation
    }
    
    //vincelli bonus
    def tail: Stream[A] = this match {
      case Empty => throw new UnsupportedOperationException("tail of empty stream")
      case Cons(h, t) => t()
    }
    def safeTail: Stream[A] = this match {
      case e@Empty => e
      case Cons(h, t) => t()
    }
    
    //EX 37. Notice how what we deal with is 0-arity functions!
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
    //The B >: A trick is a common one when you have to put in a contravariant position, such as the codomain type, a
    //variant type. But in order to keep this in the trait, where the type of the this unequivocally A, the proper way
    //is not to use type cast for sure. You can see a good example of the right way to do in the Scala library.
    def append[B >: A](a: B): Stream[B] = this match {
      case Empty => Stream.apply(a)
      case Cons(h, t) => Cons(h, () => t().append(a))
    }
    def prepend[B >: A](a: B): Stream[B] = this match {
      case Empty => Stream.apply(a)
      case c: Cons[A] => Stream.cons(a, c)
    }
    
    //EX 38. If n is more than the actual list length, no exception is thrown and the list is returned
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
      case _ => Stream.empty[A]
    }
    
    //EX 39 - teik uail (you can directly test this and the previous thanks to toList). Notice how the recursive unfold
    //is in the by-name parameter of the cons method
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty[A]
    }
    
    //zippito
    def zip[B](that: Stream[B]): Stream[(A, B)] = this match {
      case Empty => Stream.empty[(A, B)]
      case Cons(hthis, tthis) => that match {
          case Empty => Stream.empty[(A, B)]
          case Cons(hthat, tthat) => Stream.cons((hthis(), hthat()), (tthis().zip(tthat()))) 
      }
    }
    
    //This was given to the reader, no exericise. Notice that || is non-strict in its second argument
    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
    //This was a freebie too - see how f is nonstrict in its second argument
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z 
    }
    //And here's a fold-backed exists - this makes sense since foldRight can terminate its traversal early
    def foldExists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
    //ES 40 let's leverage on the same idea for a forAll
    def foldForall(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
    
    //ES 41 foldRight for takeWhile: here we define a appendIf function non-strict in the stream argument: since the
    //foldRight call is on the second argument (see above) if this is unevaluated then there is no traversal
    def foldTakewhile(p: A => Boolean): Stream[A] = {
      def appendIf(a: A, s: => Stream[A]) =
        if (!p(a)) Empty
        else Stream.cons(a, s)
      foldRight(Stream.empty[A])((a, b) => appendIf(a, b))
    }
    
    //ES 42 foldRight for headOption
    def foldHeadoption: Option[A] = {
      def extractIf(a: A, head: => Option[A]) = Some(a)
      foldRight(headOption)((a, b) => extractIf(a, b))
    }
    
    //Bonus from the solutions: 
    def foldConcat[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => Stream.cons(h,t))
    
    //ES 43 foldRight and friends. The drill again is to have the b argument unevaluated! Of course this works if the
    //functions we call are by-name in that, which is the argument is evaluated everytime inside the body, not before.
    def map[B](f: A => B): Stream[B] = foldMap(f)
    def foldMap[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b)) //OK
    def foldAppend[B >: A](x: => B): Stream[B] = foldRight(Stream.cons(x, Stream.empty[B]))((a, b) => Stream.cons(a, b)) //OK
    def f[A](a: A, p: A => Boolean) = if (p(a)) Stream(a) else Stream.empty
    def foldFilter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t) //OK; watch out it blows if exists(p) = false!
    def flatMap[B](f: A => Stream[B]): Stream[B] = foldFlatmap(f)
    def foldFlatmap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h) foldConcat t) //OK (con aiutino, grazie funzione append supra)
    
    //ES 49 - Implementing the gang as unfold
    def unfoldMap[B](f: A => B): Stream[B] = Stream.unfold(this)((a: Stream[A]) => a match {case Empty => None case Cons(h, t) => Some(f(h()), t())})
    def unfoldTake(n: Int): Stream[A] = Stream.unfold((this, n)){case (a: Stream[A], n: Int) => a match {case Cons(h, t) if n > 0 => Some((h(), (t(), n-1))) case _ => None}}
    def unfoldTakewhile(p: A => Boolean): Stream[A] = Stream.unfold((this, p)){case (a: Stream[A], p: (A => Boolean)) => a match {case Cons(h, t) if p(h()) => Some(h(), (t(), p)) case _ => None}}
    def unfoldZipwith[B](that: Stream[B]): Stream[(A, B)] = Stream.unfold((this, that)){case (a: Stream[A], b: Stream[B]) =>
      (a.headOption, b.headOption) match {
        case (Some(x), Some(y)) => Some((x, y), (a.tail, b.tail))
        case _ => None
      }
    }
    def unfoldZipall[B](that: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, that)){case (a: Stream[A], b: Stream[B]) =>
      (a.headOption, b.headOption) match {
        case (None, None) => None
        case (x, y) => Some((x, y), (a.safeTail, b.safeTail))
      }
    }
      
    //Find - Terminates as soon as the first match is found of course
    def find(p: A => Boolean): Option[A] = foldFilter(p).headOption
    
    def isEmpty = this == Empty
    
    //ES 50 - is that a prefix? (hard). [A] - try without and it will bother for contravariance
    def startsWith[A](that: Stream[A]): Boolean =
	    if (that.isEmpty) true
	    else if (this.isEmpty) false
	    else if (this.headOption == that.headOption) this.tail.startsWith(that.tail)
	    else false
    
	  //ES 51 - the stream of the suffixes!
	  def tails: Stream[Stream[A]] = Stream.unfold(this)(s => if (s.isEmpty) None else Some(s, s.tail)).append(Stream.empty[A])
	  
	  //We can use it to implement a subsequence method (freebie)
	  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
    
    //ES 52 (hard & optional) implement scanRight: just like foldRight but we return intermediate results too
    //This is a "slow" version since intermediate results are not used: this is intuitively quadratic, one scan down
    //the stream and one scan down every stream element in it
    def slowScanRight[B](b: => B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(b)(f)).append(b)
    
    //Here instead we accumulate the partials in the foldRight support stream, which enables us to compute the current
    //value starting from the previous one. Watch out for what kind of operation it makes sense to pass (well this
    //applies to the reduce/foldRight in the first place)
    def scanRight[B](b: => B)(f: (A, => B) => B): Stream[B] = tails.foldRight(Stream(b))((a, b) => if (a.isEmpty) b else Stream.cons(f(a.headOption.get, b.headOption.get), b))

    //If you tried to do this with unfold you would see that the order is reversed.

  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] 
  object Stream {
    //Smart constructor: a constructor method which ensures particular properties on the created object. Here we cache
    //as lazy the head and tail values so that we evaluate them only at first reference, with subsequent taking the
    //memoized value. See below for an example.
	  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
	    lazy val head = hd
	    lazy val tail = tl
	    Cons(() => head, () => tail)
	  }
	  //ES 44 - A constant stream
	  def constant[A](a: A): Stream[A] = {
      lazy val as: Stream[A] = Stream.cons(a, as)
      as
    }
	  //ES 45 - Now this is magic. If you want {n, f(n+1), f(n+2)...} then all you need to do is to actually compute 
	  //the first only!
	  def from(n: Int): Stream[Int] = {
	    lazy val from: Stream[Int] = Stream.cons(n, from.foldMap(_+1))
	    from
	  }
	  //ES 46 Fibs 4 Fun
	  def fibs: Stream[Int] = {
	    lazy val fibs: Stream[(Int, Int)] = Stream.cons((0, 1), fibs.map{case (a, b) => (b, a+b)})
	    Stream.cons(0, fibs map {case (_, b) => b })
	  }
	  //ES 47 Generalizing the approach: unfold
	  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = {
	    lazy val stream: Stream[Option[(A, S)]] = Stream.cons(f(s), stream.map{_.flatMap{case (_, s) => f(s)}})
	    stream.takeWhile(_ != None).map(_.get._1)
	  }
	  //Try this out with: 
	  //
	  //def f(i: Int): Option[(Int, Int)] = if (i < 10) Some((i+1, i+1)) else None
	  //Stream.unfold[Int, Int](0)(f)
	  //
	  //This is a corecursive function: the focus is to generate output rather than process input. Notice how the
	  //function is active only as long as we don't hit None - try to insert some println and see; the "productivity" 
	  //of a function is referred to as cotermination.
	  
	  //ES 48 - the utilities above as unfold. Notice how for the first two S doesn't really matter and we have S=A but
	  //for the fibs the stream is Int but what we compose on is (Int, Int)
	  def unfoldConstant[A](a: A) = unfold(a)(_ => Some((a, a)))
	  def unfoldFrom(n: Int) = unfold(n-1)(n => Some((n+1, n+1)))
	  def unfoldFibs = Stream.cons(0, unfold((0, 1)){case (a, b) => Some(a+b, (b, a+b))}) 
	  
	  def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
  //In the following:
  def expensive() = println("expensive")
  val tl = Stream(())
  val x = Cons(() => expensive(), () => tl)
  val h1 = x.headOption
  val h2 = x.headOption
  //expensive() is called and computed twice, simply because we call h() in headOption twice. If we have the following:
  val y = Stream.cons(expensive(), tl)
  val i1 = y.headOption
  val i2 = y.headOption
  //this happens only once!
  
  //An infinite stream of one's 
  lazy val ones: Stream[Int] = Stream.cons(1, ones)
  //All of the stream functions work on infinite lists too of course, since the whole structure is never unfolded this
  //is not a problem.
  assert(ones.take(5).toList == List(1, 1, 1, 1, 1))
  assert(ones.foldExists(_ % 2 != 0))
  assert(ones.foldMap(_ + 1).foldExists(_ % 2 == 0))
 
  
  //We start to explore the design of functional APIs by looking at an example of something that it is not functional,
  //the random number generation utility in Scala
  val rng = new Random
  val r1 = rng.nextInt
  val r2 = rng.nextInt
  val r3 = rng.nextInt 
  //Now the rng object has some internal state, initialized with some function of the seed value, which mutates every
  //time as new randoms from the stream are picked. This instance is not referentially transparent, of course because
  //if in two identical programs we have the occurence and we substitute both for their values things change, namely
  //we cannot say the generated numbers will be the same, in fact we really expect them to be even different. The
  //state change is actually a side effect which comes with the call, and this side effect prepares the new internal
  //state of the object. 
  //So let's define some interface to return not only the number but the mutated state too, explicitly:
  
  trait RNG {
    
    type Rand[+A] = RNG => (A, RNG)
    
    def nextInt: (Int, RNG)
    
    
  }
  
  object RNG {
    
    type Rand[+A] = RNG => (A, RNG)
    
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
	    val (a, rng2) = s(rng)
	    (f(a), rng2)
    }
    
    //ES 56 Generalization of map
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }
        
    //EX 58 flatMap
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f.andThen{case (a, rng) => g(a)(rng)}  
    
    //EX 59 map and map2 in terms of flatMap - that's why flatMap can be harder to implement, 'cause it's the main man
    //Notice that, to my great surprise, andThen is not defined only for simple functions, but we can easily get it
    //done by tuple wrapping. 
    def flatmapMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(f.andThen(unit(_)))
    def flatmapMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      flatMap(unit((a, b)))(f.tupled.andThen(unit(_)))(rngb)
    }
    
    //We can do for example:
    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))
    
    def nextInt(rng: RNG): (Int, RNG) = rng.nextInt
    
    //EX 52 - Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue 
    //(inclusive). Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn't have a 
    //non-negative counterpart
    @tailrec
    def nextPosInt(rng: RNG): (Int, RNG) = {
      val (n, nrng) = rng.nextInt
      if (n != Integer.MIN_VALUE) (abs(n), rng)
      else nextPosInt(nrng)
    }
    //This generates in a given range instead; the smaller the range the slower the method of course
    def nextRangeInt(rng: RNG)(start: Int, stop: Int): (Int, RNG) = {
      val (n, nrng) = rng.nextInt
      if (n >= start && n <= stop) (n, rng)
      else nextRangeInt(nrng)(start, stop)
    }
    
    //Using the map to get it even too
    def nextEvenPosInt: Rand[Int] = map(nextPosInt)(n => n - n % 2)
    
    //EX 53 The real
    def nextDouble(rng: RNG): (Double, RNG) = {
      val (n, nrng) = nextPosInt(rng)
      (n.toDouble/Integer.MAX_VALUE, nrng)
    }
    
    //ES 55 A more elegant map-driven nextDouble
    def mapNextDouble: Rand[Double] = map(nextPosInt)(n => n.toDouble/Integer.MAX_VALUE)
    
    //EX 54 Tooples. Notice how we must always make use of the provided state rng by passing it along as an argument
    //or invoking its nextInt method
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (n, nrng) = rng.nextInt
      ((n, nextDouble(rng)._1), nrng)
    }
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (nr, nrng) = intDouble(rng)
      (nr.swap, nrng)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, nrng1) = nextDouble(rng)
      val (d2, nrng2) = nextDouble(nrng1)
      val (d3, nrng3) = nextDouble(nrng2)
      ((d1, d2, d3), nrng3)
    }
    //With the map2 function above the first two simplify as:
    def bothIntDouble: Rand[(Int, Double)] = both(nextInt, nextDouble)
    
    //EX 55 List of ints; def unfold[A, S](s: S)(f: S => Option[(A, S)]) is magiccc
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      def f(state: (Int, RNG)): Option[((Int, RNG), (Int, RNG))] = {
        val (count, rng) = state
        if (count > 0) {
          val s@(n, nrng) = rng.nextInt
          Some((s, (count-1, nrng)))	
        }
        else
          None
      }
      val (ints, rngs) = Stream.unfold((count, rng))(f).toList.unzip
      (ints, rngs.last)
    }
    
    //EX 57 (hard as fuck): sequence. This is best done with a fold not an unfold since we already have all of the
    //info. We can of course use it to get a list of random ints (see above under ints()).
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { 
      if (fs.isEmpty) unit(List.empty[A])
      else rng => fs.foldRight((List.empty[A], rng)){(x, y) => val (as, rng) = y; val (a, nrng) = x(rng); (a :: as, nrng)}
    }
      
    
    
    //EX 57bis: redefine random ints list with sequence
    def sequenceInts(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))
    
    //EX 58bis non negative and less than
    def nextPosInt(n: Int)(rng: RNG): Rand[Int] = flatMap(nextPosInt)(u => unit(u % (n+1)))
    
    //EX BonusTrack: magic recursion
    def nextIntWithParity(even: Boolean)(rng: RNG): Rand[Int] = RNG.flatMap(rng => nextInt(rng))(n => if (even && n%2==0) RNG.unit(n) else nextIntWithParity(even)(rng))
  }
  
  //An implementation; for the record, an LCG, the same algo Scala uses:
  case class Simple(seed: Long) extends RNG {
	  def nextInt: (Int, RNG) = {
	    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL //& is bitwise and here, and these are hex's
	    val nextRNG = Simple(newSeed)
	    val n = (newSeed >>> 16).toInt //>>> is right binary shift with zero fill
	    (n, nextRNG)
	  } 
	}
  val frng = Simple(42)
  val (n1, frng1) = frng.nextInt
  val (n2, frng2) = frng1.nextInt
  
  
  //The really great thing is that, for a given seed, we will always get the same sequence! Now this is purity allows
  //us not only to compose functionally but it also brings us closer to the original formulation: the algorithm is
  //deterministic.
  
  //A state-transforming function is usually of form S => (A, S), where for some input state s we get back a value a
  //and a new state s'; above, the state is a generator. Such functions are called state actions/transitions too and
  //we will see how to combine them. Let's start by defining an alias for such transformation type, Rand. The first
  //and simplest combinator is a curried function which gives us a constant function of some type A where the argument
  //generator is just returned back. For example:
  assert(RNG.unit(3333)(frng)._1 == 3333)
  //This is basically the identity transformator. The map combinator instead applies the given function to the return
  //of the Rand it operates on:
  assert(RNG.map(RNG.unit(3333))(_*2)(frng)._1 == 6666)

  //The companion functions above don't have anything to do with the concept of random number generator, but rather
  //with state handling and transformation. The general definition of map for this kind of abstraction is for example:
  //def map[S, A, B](a: S => (A, S))(f: A => B): S => (B, S) = ???
  //where S can be substituted for RNG, and a more general state of the same form as Rand is:
  type State[S, +A] = S => (A, S)
  //which turns the definition above into:
  def stateMap[S, A, B](a: State[S, A])(f: A => B): State[S, A] = ???
  
  //We can also represent the state as a typeclass, and implement more general versions of the combinators above
  //EX60: the class is basically a wrapper around the alias function definition. Notice again how we go imperative
  //with the vals, but no side effects here! The fundamental operations for this State wrapper is the possibility to
  //get and set the state of course.
  case class StateClass[S, +A](r: State[S, A]) {
    def map[B](f: A => B): StateClass[S, B] = this.copy(r = s => r(s) match {case (a, s) => (f(a), s)})
    def map2[B, C](rb: StateClass[S, B])(f: (A, B) => C): StateClass[S, C] = this.copy(r = s => {
      r(s) match { case (a, ns) => rb.r(ns) match { case (b, nns) => (f(a, b), nns) } }
    })
    def flatMap[B](g: A => StateClass[S, B]): StateClass[S, B] = StateClass(r.andThen{case (a, s) => g(a).r(s)})
  }
  object StateClass {
    def unit[S, A](a: A): StateClass[S, A] = StateClass(r = s => (a, s))
    def get[S]: StateClass[S, S] = StateClass(s => (s, s))
    def set[S](s: S): StateClass[S, Unit] = StateClass(_ => ((), s))
  }
  //So the takeaway for stateful functional programs is: pure functions which accept a state and they return it
  //modified together with the computed value.


  //We are interested now in defining a purely functional API for parallel computation. Of course Scala already has
  //support in this sense, and cool too, for example any collection can be turned into parallel. Java has support too
  //but way less advanced, and trouble to work with, than Scala: for example threads map onto physical threads and it
  //is not easy to keep track of their state, Futures do not compose.
  //Let's define:
  trait Par[A]
  object Par {
    def apply[A](a: A): Parallel[A] = unit(a)
    def unit[A](a: => A): Parallel[A] = ???
    def get[A](a: Parallel[A]): A = ???
    //EX61: combining the results of two parallel computations. Notice how arguments are nonstrict, so that we don't
    //have any execution start bias. 
    def map2[A, B, C](a: => Parallel[A], b: => Parallel[B])(f: (A, B) => C): Parallel[C] = unit(f(get(a), get(b)))
    //Giving the programmer the possibility to choose about threading
    def fork[A](a: => Parallel[A]): Parallel[A] = ???
    //if we introduce this lazyUnit, the definition above for unit loses the '=>', and map2 too (see below)
    def lazyUnit[A](a: => Parallel[A]) = fork(a)
    def run[A](a: Parallel[A]): A = ??? //substitute for get (below)
  }
  //unit accepts an unevaluated value to be evaluated/executed in another thread, we don't care now if actual or
  //logical; get retrieves the computed value of this execution container. What about not yet their implementation, 
  //but their behavior? For unit, consider f(Par(1), Par(2)), the arguments being strict. Now Scala evaluates left 
  //to right, and strict evaluation implies that the value arguments are computed one by one. If the computation 
  //wasn't triggered at object instatiation, by which we mean that the constructor immediately returns after launchin 
  //computations, then what we would have here is actually a sequential flow!
  //About get, this is supposed to retrieve the value of a given Par. But when we call get, the computation might be
  //ongoing! In that case there's no value, and we are forced to return null or an exception. A more pratical behavior
  //would be to have the method to wait on the computation: calling get blocks until the result is ready to be
  //returned. This behavior let's us build a map2 combiner already. There is some resemblance to Future but the value
  //method interrupts the computation; our get looks more like Await.result.
  //Is it always the case that we want to spawn off a new thread for every unit? It might be the case that it is not
  //worth the cost because there is nothing to compute, say unit(a) with a constant. We therefore define a fork meth
  //whose purpose is to instrument the computation in a separate thread. This way it is up to the programmer to choose
  //where to go; the concept heavily resembles Future's successful and failed methods. We can therefore define a new
  //kind of lazy-delayed unit, as opposite to the previous one which is now supposed to be on-thread, not parallel.
  //Accordingly, map2 loses its '=>''s too: why not letting the programmer opt for computing the first Par immediately,
  //if it comes from a unit, and not a lazyUnit? 
  //We are going towards a common conceptual pattern here: we want to abstract out from execution flavors in our abs
  //while giving the user a sufficiently ergonomic API. The problem here is about names and intended uses. 
  //Let us ask this question again: shall the computation start when fork is called, or does this method have to just
  //attach some description to Par in order to have it run on the threadpool? If we now choose the latter, we need to
  //pass the responsibility to some other method. The get method has the right signature but the wrong name, what we
  //want now this method to do is to rather start some Par, so we'll name it run. Ok so the final API will be:
  
  type Parallel[A] = ExecutorService => JavaFuture[A]
  
  object JavaFuture {
    def successful[A](a: A) = new JavaFutureTask(new Callable[A]{def call = a})
  }
  
  object Parallel {
    //This just returns a Future.successful
    def unit[A](a: A): Parallel[A] = _ => JavaFuture.successful(a)
    //Execution is on-thread here, as we said it is with fork that we command for parallelism. No timeout handling.
    def map2[A, B, C](a: Parallel[A], b: Parallel[B])(f: (A, B) => C): Parallel[C] = es => {
      val af = a(es)
      val bf = b(es)
      JavaFuture.successful(f(af.get(), bf.get()))
    }
    //Tadaaa, this is when all the fun is. Callable is simply an interface with a call method, and it is meant for
    //extra-thread execution, like Runnable but with a value. We pass it to the threadpool manager, which dispatches
    //it. The thread which will execute it though will block, infact there is a .get; this is not great and we are
    //going back to it later.
    def fork[A](a: => Parallel[A]): Parallel[A] = es => es.submit(new Callable[A]{def call = a(es).get}) 
    def lazyUnit[A](a: => A): Parallel[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Parallel[A]): JavaFuture[A] = a(s)
    //EX 61 - turn any function into a parallel one; notice how this is possible thanks to the => in lazyUnit
    def asyncF[A, B](f: A => B): A => Parallel[B] = a => lazyUnit(f(a))
    //Sorting; the only way we have so far to apply something inside a map is with map2, of course we can just ignore
    //the second argument. This isn't cheating, just map2 >> map
    def sortParallel[A : Ordering](parList: Parallel[List[A]]): Parallel[List[A]] = map2(parList, unit(()))((a, _) => a.sorted)
    //Our all-time-favorite
    def map[A, B](pa: Parallel[A])(f: A => B): Parallel[B] = map2(pa, unit(()))((a,_) => f(a))
    //now sorting simply becomes:
    def mapSortParallel[A : Ordering](parList: Parallel[List[A]]): Parallel[List[A]] = map(parList)(_.sorted)
    //EX 62 sequence the boss. Once again when you think of a sequence talk with a fold.
    def sequence[A](l: List[Parallel[A]]): Parallel[List[A]] = 
      if (l.isEmpty) unit(List.empty[A])
      else l.foldRight(unit(List.empty[A])){case (pa, as) => map2(pa, as)((a, as) => a :: as)} 
    //parMap is used to parallely map f on l; it will spawn as many computations as the length of the list
    def parMap[A, B](l: List[A])(f: A => B): Parallel[List[B]] = fork {
      val fbs: List[Parallel[B]] = l.map(asyncF(f))
      sequence(fbs)
    }
    //EX 62 parFilter - single-thread work
    def parFilterSingle[A](l: List[A])(f: A => Boolean): Parallel[List[A]] = lazyUnit(l.flatMap(a => if (f(a)) List(a) else Nil))
    //real parallelism
    def parFilterReal[A](l: List[A])(f: A => Boolean): Parallel[List[A]] = {
      l.foldRight(unit(List.empty[A])){(a, b) => 
        val r: Parallel[List[A]] = lazyUnit(if (f(a)) List(a) else Nil)
        map2(r, b)(_:::_)
      }
    }
    //Continuation-style blocking method:
    def choice[A](cond: Parallel[Boolean])(t: Parallel[A], f: Parallel[A]): Parallel[A] = es => if (run(es)(cond).get) t(es) else f(es)
    //EX 63 n-generalization
    def choiceN[A](n: Parallel[Int])(choices: List[Parallel[A]]): Parallel[A] = es => run(es)(choices(run(es)(n).get))
    //choice above becomes:
    def choice2[A](cond: Parallel[Boolean])(choices: List[Parallel[A]]): Parallel[A] = es => if (run(es)(cond).get) run(es)(choiceN(unit(1))(choices)) else run(es)(choiceN(unit(0))(choices))
    //In a similar way we can implement a choice method with a Map or Vector container. In general we can use an
    //IndexedSeq. Less elegantly we can also have something like:
    def chooser[A, B](pa: Parallel[A])(choices: A => Parallel[B]): Parallel[B] = es => run(es)(choices(run(es)(pa).get))
    //and implement choice N like:
    def chooserN[A](n: Parallel[Int])(choices: List[Parallel[A]]): Parallel[A] = chooser(n)(choices)
    //EX 64 join aka flatten
    def join[A](a: Parallel[Parallel[A]]): Parallel[A] = es => run(es)(map(a)(run(es)(_).get))
    //EX 64 bisse flatMap
    def flatMap[A, B](a: Parallel[A])(f: A => Parallel[B]) = join(map(a)(f))
    //Required below for a trampolined blockfree IO interpreter
    def async[A](run: (A => Unit) => Unit): Parallel[A] = ???
  }
  
  //At first approximation, Parallel is just a function that accepts some threadpool and returns a computation to be:
  //see how it first started as an alias for a future and then now it ends up using it. I haven't found a clean way
  //to use the Scala stuff, so I'll follow the textbook here using Java's; the biggest rock I hit is that none of EC
  //and friends accepts a Callable and returns a Future (ExecutionContextExecutorService has a submit method accepting
  //a callable but it returns a javafute. This we have defined is a pure api in the sense that it has no side-effects
  //per se, even if of course at his core it has, because the java future machinery we deal with is non functional.
  
  //Our API as an algebra. An algebraic structure is a set equipped with a number of operations; a famous example is
  //N with +, naturals with addition. In our case, the set is all of the possible types, represented in an
  //anonymous fashion by A, and the operations the methods themselves. Now an algebra respects one or more rules; for
  //example (N, +) has an identity and the plus is associative and commutative, this takes the particular name of
  //monoid. The first law for us could be:
  def mapId[A](pa: Parallel[A]) = assert(Parallel.map(pa)(identity) == pa)
  //from which we can derive the general case holding for f.
  //Another interesting law is:
  def forkId[A](pa : Parallel[A]) = assert(Parallel.fork(pa) == pa)
  //reminding us that for is concerned with the side-effect execution of the computation, this does not show at API
  //user level
  //Given an arbitrary type A our combinators work with a composite type Parallel[A], which is a function whose arg
  //is the ExecutionService, and this is an interface, so arbitrary implementations are possible too. Of course we
  //have to constraint to the sound ones, meaning that we cannot expect of course the API to work with a broken ES.
  //The following property comes into play then:
  def equal[A](e: ExecutorService)(p1: Parallel[A], p2: Parallel[A]): Boolean = p1(e).get == p2(e).get
  //which tells us: two parallels are equivalent only if they yield the same result for any 
  //One of the standard Java implementations accept a parameter to work with a fixed-size number of threads. Now we
  //can write the following:
  val a = Parallel.lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(equal(S)(a, Parallel.fork(a)))
  //This results in a deadlock, because we are nesting two callables and the thread is used by the container, which
  //waits on the actual execution, val a. In situations like this, you either add constraints to what the function
  //expects - so here we are talking about accepting only ExecutorService's of unbounded poolsize, or redefine a
  //a little your abstractions.
  
  
  //Let's embark now on the journey to defining and (partially) implementing a ScalaCheck-like testing library.
  type SuccessCount = Int
  type FailedCase = String
  trait Prop { 
    def check: Boolean
    //EX 65 and-composition, helped by the truth and the false atoms below: if the property is false, whatever other
    //property, in an end, will result in the false too; if it is true, then it depends on the other property
    def &&(p: Prop): Prop = if (check) p else F
    //Now the check method tells us if the property checked or not, but this isn't enough if we also want to know 
    //about the counterproof instances for example when it fails. So we could have:
    def richCheck: Either[(FailedCase, SuccessCount), SuccessCount]
    //where type aliases increase the readability, and for a failure we return a description of the first input that
    //invalidated the property, while returning the count of the successful samples in both cases. We don't have a
    //typed return here because at the moment we assume the caller wouldn't make any use of the type info. In general
    //a method, function, routine or whatever should always ask for the minimum information it needs and return the
    //minimum information that the caller needs.
  }
  //Reifications for true and false simbols for &&, see above
  object F extends Prop {
    def check: Boolean = false
    override def &&(p: Prop) = this
    def richCheck: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  }
  object T extends Prop {
    override def check: Boolean = true
    override def &&(p: Prop) = p
    def richCheck: Either[(FailedCase, SuccessCount), SuccessCount] = ???
  }
  //Investigating on Prop, one other piece of information we would like to have is how many tests we expect the
  //property to pass to be considered true. Let's turn it into a case class for simplicity and have:
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]
  case class Property(run: (RNG, TestCases) => Result) {
    //EX 70. Remember that if the property check is successful then, with the type Result, we get a None, otherwise
    //a Some. So we run this property; if it is a Some, then we get it, no need to compute the second property, we
    //failed already; it is a None then let's check the second property too. See the duality: with && we stop if we are
    //false, with || if we are true.
    def &&(p: Property): Property = Property { case (rng: RNG, n: TestCases) => run(rng, n).orElse(p.run(rng, n)) }
    def ||(p: Property): Property = Property { case (rng: RNG, n: TestCases) => if (run(rng, n).isEmpty) None else p.run(rng, n) }   
  }
  object Property {
    //Property validator - see below for an example Property
    def run(p: Property, maxSize: Int = 100, testCases: Int = 100, rng: RNG = Simple(System.currentTimeMillis)): Unit =
      p.run(rng, testCases) match {
		    case Some((msg, n)) =>
		      println(s"! Falsified after $n passed tests:\n $msg")
		    case None =>
		      println(s"+ OK, passed $testCases tests.")
      }
  }
  //where Result is not an Either anymore since the right value, SuccessCount, equals TestCases. None thus represents
  //the absence of a failure. Property also needs an rng because its property-verifying machinery needs to generate
  //random tests through Gen. We are now ready to implement forAll! See below in SC
  
  //A Gen[A] is a tool which knows how to create objects of type A. We can make awesome use of the State and RNG
  //abstractions from above for this
  //ex 64-65: unit, choose, listOfN
  case class Gen[A](sample: State[RNG, A]) {
    //EX 67 - flatMap (awesome!)
    def flatMap[B](f: A => Gen[B]) = Gen { rng => 
      val (a, nrng) = sample(rng)
      val gb = f(a)
      gb.sample(nrng)
    }
    //EX 70 Return this an unsized generator (see below)
    def unsized: SGen[A] = SGen(_ => this)
  } 
  object Gen {
    def unit[A](a: A): Gen[A] = new Gen(RNG.unit(a))
    //EX 66 - the generation method. We want a generator which samples the set {start,..., stop}
    def choose(start: Int, stop: Int): Gen[Int] = {
      //Now this is not really functional because if the condition is false an exception is thrown. Such statements
      //are ok for instructive or debug purposes but not in a clean functional world
      require(start <= stop)
      Gen(rng => RNG.nextRangeInt(rng)(start, stop))
    }
    //EX 72 - nonempty lists
    def listOf1[A](g: Gen[A]): Gen[List[A]] = listOfN(1, g)
    //EX 67 List generator
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen{rng => 
      @tailrec
      def f(n: Int)(acc: (List[A], RNG)): (List[A], RNG) =
        if (n == 0) acc
        else {
          val (as, rng) = acc
          val (na, nrng) = g.sample(rng)
          f(n-1)((na :: as, nrng))
        }
      f(n)(Nil, rng)
    }
    //EX 67 - two random ints in an interval of the same parity via flatMap (this is a mess since the combinators for RNG are in the RNG.companion RNG.object)
    def sameParity(from: Int, to: Int): Gen[(Int, Int)] = Gen.choose(from, to).flatMap(n => Gen(rng => RNG.flatMap(RNG.unit(n))(n => RNG.flatMap(RNG.nextIntWithParity(n%2==0)(rng))(m => RNG.unit((n, m))))(rng)))
    //EX 67 a flatMap - based listOfN
    def flatmaplistOfN(n: Int): Gen[List[Int]] = Gen.unit(n).flatMap(n => Gen(rng => RNG.ints(n)(rng)))
    //EX 69 - generator coinflip
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen.choose(0, 1).flatMap(n => if (n == 0) g1 else g2)
    
  }
  
  object SC {
    private def randomStream[A](as: Gen[A])(rng: RNG): Stream[A] = ???
    private def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    //Implementation of forAll, see above
    def forAll[A](as: Gen[A])(f: A => Boolean): Property = Property { case (rng: RNG, n: TestCases) => 
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) None else Some((a.toString, i))
        } catch { case e: Exception => Some((buildMsg(a, e), i)) }
      }.find(_.isDefined).getOrElse(None)
    }
  }
  
  //In order to facilitate debugging, once we find a failing test case we would like to know about the minimum failing
  //case too. In general we have two techniques for sampling the test space:
  //-shrinking: random sampling, and once we have a faulty testcase, we reduce its size until we get to a green case; 
  // it is dependent on the data type (with a collection for example one dimension is the size of it, the other the 
  // type it contains)
  //-sized generation: we start the generation of cases from the bottom up, so that the first failing sample is the
  // least complex; the "jump" size is the main parameter
  
  //A sized generator:
  case class SGen[A](forSize: Int => Gen[A])
  object SGen {
    //EX 71 - sized list
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g)) 
  }
  
  //Finally, an example:
  val smallInt = Gen.choose(-10, 10)
  val maxProp = SC.forAll(Gen.listOfN(100, smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }
 
  //EX 72 - a property for sorting
  val sortProp = SC.forAll(Gen.listOfN(100, smallInt)) { l => l == l.sorted }
  
  //In the previous chapter we formulated the following property for Parallel
  //assert(Parallel.map(pa)(identity) == pa)
  //where pa is some Parallel
  
  //Now at a first attempt this could be expressed as:
  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = SC.forAll(Gen.unit(Par.unit(1)))(i => Parallel.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
  //and this, even if correct, is not very clear to read and shows an hardcoded constant, 1, which is not really
  //important per se, any number and operation would do.
  //Let us introduce the following:
  def check(p: => Boolean): Property = {
    lazy val result = p
    SC.forAll(Gen.unit(()))(_ => result)
  }
  //with which we can rewrite our prop as:
  val p1_improved = check { 
    val p = Parallel.map(Par.unit(1))(_ + 1)
    val p2 = Parallel.unit(2)
    p(ES).get == p2(ES).get
  }
  //Now the last step is easily liftable by introducing:
  def equal[A](p1: Parallel[A], p2: Parallel[A]): Parallel[Boolean] = Parallel.map2(p1,p2)(_ == _)
  //so that we can suggest:
  val p3 = check {
    equal (
	    Parallel.map(Par.unit(1))(_ + 1),
	    Parallel.unit(2)
    ) (ES) get
  }
  
  //EX 73 The fork property
  val pc = check {
    equal (
      Parallel.fork(Par.unit(1)),
      Par.unit(1)
    ) (ES) get
  }
  
  //The only thing our library is lacking so far is function generators. Of course we can always have something static
  //like:
  val isEven = (i: Int) => i%2 == 0
  val takeWhileProp = SC.forAll(Gen.listOfN(100, smallInt))(l => l.takeWhile(isEven).forall(isEven))
  //This is for sure doable anyway: of course we can always have constant functions, for which the argument is always
  //ignored, or something more exoteric like the hashcode modulo the collection length for example...
  
  
  //The new fun adventure is the creation of a parser library. Parsers come in two flavors:
  //-full/classic: the parser is written to read some document with a particular structure, and that only, and it
  // it works by reading down the document, saving piece by piece
  //-combinator: a combinator is a "parser of parser", in the sense that it uses a number of simple parser to read a
  // a document
  
  //Our focus is on parser combinators of course, and you can see that a PC is actually a higher-order function. For
  //example a JSON parser combinator requires a number of simpler parsers such as an object, pair and type parsers.
  
  //EX 80 - erroring
  type ParseError[A] = (A, Int, String) //where A is the element at which the Parser[A] fails and Int is its position
  object ParseError {
    def multipleErrors[A](errs: ParseError[A]*): List[ParseError[A]] = ???
    def checkValue[A](err: ParseError[A], a: A): Boolean = err._1 == a
    def toString[A](err: ParseError[A]): String = ???
  }
  
  trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
    
    type ParseResult[A] = Either[ParseError[A], A]
    
	  def run[A](p: Parser[A])(input: String): ParseResult[A]
	
	  implicit def string(s: String): Parser[String]
	  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
	  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
	    ParserOps[String] = ParserOps(f(a))
	
	  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

	  /*
	  * A default `succeed` implementation in terms of `string` and `map`.
	  * We leave `succeed` abstract, since `map` is defined below in terms of
	  * `flatMap` and `succeed`, which would be a circular definition! But we include
	  * the definition here in case implementations wish to use it
	  * (say if they provide a custom implementation of `map`, breaking the cycle)
	  */

	  def succeed[A](a: A): Parser[A] = string("") map (_ => a) //dummy !
	
	  def slice[A](p: Parser[A]): Parser[String] //returns the portion of the string examined by the parser, if success
	
	  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
	    if (n <= 0) succeed(List())
	    else map2(p, listOfN(n-1, p))(_ :: _)
	
	  def many[A](p: Parser[A]): Parser[List[A]] =
	    map2(p, many(p))(_ :: _) or succeed(List())
	
	  //Nonstrict so that this is a ||, short circuit
	  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
	  //EX 79 conditional ||: we try p2 only if the condition on p1 results true
	  def or[A](p1: Parser[A], p2: => Parser[A])(p: ParseResult[A] => Boolean): Parser[A]
	
	  //This time around flatMap is the primitive
	  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

	  /*
	  These can be implemented using a for-comprehension, which delegates to the `flatMap` and `map` implementations we've provided on `ParserOps`, or they can be implemented in terms of these functions directly.
	  */
	  //EX 76 (for free)
	  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = //notice the nonstrictness
	    flatMap(p)(a => map(p2)(b => (a,b))) //runs the two parsers in sequence
	
	  //Second argument is nonstrict; this makes functions like many above possible
	  //EX 76 (for free)
	  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
	    for { a <- p; b <- p2 } yield f(a,b)
	    
	  //EX 74 products for mapS
	  def map2product[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = product(p, p2) map f.tupled
	
	  //Recognizes the regular language a*b, returning the sizes
	  def example1 = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
	  
	  //EX 75 Recognizes the context sensitive grammar n x a_0 x a_1 x ... x a_n, where a is given. Context sensitivity 
	  //here means that the second parser has the first as input.
	  def example2[A](a: A) = "\\d".r.flatMap(s => listOfN(Integer.parseInt(s), succeed(a)))
	  
	  //EX 77 (for free)
	  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
	    flatMap(a)(f andThen succeed)
	
	  def label[A](msg: String)(p: Parser[A]): Parser[A]
	
	  def scope[A](msg: String)(p: Parser[A]): Parser[A]
	
	  def attempt[A](p: Parser[A]): Parser[A]
	    
	  //EX 75 (to transform a regex into a parser, es "\d" -> 0 | 1 | 2 | ...
	  implicit def regex(r: Regex): Parser[String]

	  case class ParserOps[A](p: Parser[A]) {
	    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2) // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
	    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
	
	    def map[B](f: A => B): Parser[B] = self.map(p)(f)
	    def many = self.many(p)
	    //EX 74 BIS many1 in terms of many
	    def many1: Parser[List[A]] = map2(p, p.many)(_ :: _)
	
	    def slice: Parser[String] = self.slice(p)
	
	    def **[B](p2: => Parser[B]): Parser[(A,B)] =
	      self.product(p,p2)
	    def product[B](p2: => Parser[B]): Parser[(A,B)] =
	      self.product(p,p2)
	
	    def flatMap[B](f: A => Parser[B]): Parser[B] =
	      self.flatMap(p)(f)
	
	    def label(msg: String): Parser[A] = self.label(msg)(p)
	
	    def scope(msg: String): Parser[A] = self.scope(msg)(p)
	
	    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
	  }
    object Laws {
	    private def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Property =
	      SC.forAll(in)(s => run(p1)(s) == run(p2)(s))
	    def mapLaw[A](p: Parser[A])(in: Gen[String]): Property = 
	      equal(p, p.map(a => a))(in)
	    def successLaw[A](a : A, s: String) = assert(run(succeed(a))(s) == Right(a))
	  }
  }
  
  //I do not intend to implement the json parser as requested by exercise 78 not because it is hard but because this
  //goes beyond the scope of doing this book. I reserve the right to do the same for future infinite exercises like
  //this one here, without notice.
  
 
  
  
  //A monoid is defined as a set A equipped with an operation + s.t.:
  //there exists an e in A s.t. for any a in A a + e = a holds (e is the identity element)
  //+ is associative, the parenthesization does not matter, namely x + (y + z) = (x + y) + z
  //We can define:
  trait Monoid[A] { self =>
    def zero: A
    def +(a1: A, a2: A): A
    object Laws {
      def id(a: A) = assert(self.+(a, zero) == a)
      def assoc(a1: A, a2: A, a3: A) = assert(self.+(a1, self.+(a2, a3)) == self.+(self.+(a1, a2), a3))
    }
  }
  //EX 79-80-81 examples
  object Monoid {
	  def stringMonoid = new Monoid[String] {
	    def zero = ""
	    def +(s1: String, s2: String) = s1+s2
	  }
	  def intSumMonoid = new Monoid[Int] {
	    def zero = 0
	    def +(i1: Int, i2: Int) = i1+i2
	  }
	  def intMulMonoid = new Monoid[Int] {
	    def zero = 1
	    def +(i1: Int, i2: Int) = i1*i2
	  }
	  def listMonoid[A] = new Monoid[List[A]] {
	    def zero = Nil
	    def +(l1: List[A], l2: List[A]) = l1++l2
	  }
	  def optionMonoid[A](implicit m: Monoid[A]) = new Monoid[Option[A]] {
	    def zero = None
	    def +(o1: Option[A], o2: Option[A]) = (o1, o2) match {
	      case (Some(a1), Some(a2)) => Some(m.+(a1, a2))
	      case (None, s) => s
	      case (s, None) => s
	      case _ => zero
	    } 
	  }
	  //EX 89 As it is Either is a monoid, but there the choice is arbitrary when it comes to choose the zero, the bias.
	  //It cannot form a product monoid in the sense of below since this is a xor not an end, you choose either A or B,
	  //not both
	  def eitherMonoid[A, B](implicit ma: Monoid[A], mb: Monoid[B]) = new Monoid[Either[A, B]] {
	    def zero = Left(ma.zero)
	    def +(e1: Either[A, B], e2: Either[A, B]) = (e1, e2) match {
	      case (Right(b1), Right(b2)) => Right(mb.+(b1, b2))
	      case (Left(a1), Left(a2)) => Left(ma.+(a1, a2))
	      case (r: Right[B], _) => r
	      case (_, r: Right[B]) => r
	    }
	  }
	  //EX 81 - a function from A to A is sometimes called an endofunction
	  def endoMonoid[A] = new Monoid[A => A] {
	    def zero = identity[A]
	    def +(f: A => A, g: A => A) = f compose g
	  }
	  //EX 90 A monoid for functions in the monoid codomain
	  def functionMonoid[A, B](mb: Monoid[B]) = new Monoid[A => B] {
	    def zero = (a: A) => mb.zero
	    def +(f: A => B, g: A => B) = (a: A) => mb.+(f(a), g(a))
	  }
	  //EX 82 - monoid instance for String that inserts spaces between words unless there already is one, and trims 
	  //spaces off the ends of the result
	  def trimMonoid = new Monoid[String] {
	    def zero = stringMonoid.zero
	    def +(s1: String, s2: String) = stringMonoid.+(stringMonoid.+(s1.trim, " "), s2.trim).trim
	  }
	  //Freebie (but used in ex 94)
	  def mapMergeMonoid[K, V](mv: Monoid[V]) = new Monoid[SMap[K, V]] {
	    def zero = SMap()
	    def +(a: SMap[K, V], b: SMap[K, V]) = a.map { case (k, v) => (k, mv.+(v, b.get(k) getOrElse mv.zero)) }
	  }
	  //EX 83 folding a list with a monoid
	  def concatenate[A](as: List[A], m: Monoid[A]): A = as.fold(m.zero)(m.+)
	  //EX 84 this is useful if we have a monoid on B and a function from A to B; this can be implemented more efficiently
	  //(see below)
	  def foldMap[A, B](as: List[A], mb: Monoid[B])(f: A => B): B = concatenate(as.map(f), mb)
	  //EX 85 some combinations
	  def foldrightFoldmap[A, B](as: List[A], mb: Monoid[B])(f: A => B): B = as.foldRight(mb.zero)((a, b) => mb.+(b, f(a)))
	  def foldmapFoldright[A, B](as: List[A], mb: Monoid[B])(f: (A, B) => B): B = as match {
      case a :: as => mb.+(foldMap(List((a, mb.zero)), mb)(f.tupled), foldmapFoldright(as, mb)(f)) //almost magic
      case _ => mb.zero
    }
	  //EX 86 With an associative operation we can either fold right or left, freely, that's actually why we can use
	  //the fold method safely. But we can actually do something called balanced folding, which is a fold amenable to
	  //parallelization:
	  //+(+(a, b), +(c, d))
	  //as opposed to a right which looks like:
	  //+(a, +(b, +(c, d)))
	  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
	    if (v.size <= 1) v.headOption.map(f).getOrElse(m.zero)
	    else {
	      val (v1, v2) = v.splitAt(v.length/2)
	      m.+(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
	    }
	  //Fact: the product of two monoids is still a monoid. Let (M,+) and (N,*) be two monoids then ((M, N), o) is a monoid
    //too where forall a=(m, n), b=(m', n') in (M, N) a o b = (m+m', n*n')
	  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]) = new Monoid[(A, B)] {
	    def zero = (ma.zero, mb.zero)
	    def +(x: (A, B), y: (A, B)) = (ma.+(x._1, y._1), mb.+(x._2, y._2))
	  }
  }
	  //EX 86-87 We want to implement as a monoid a word counter for potentially very large strings. The big sizes call
	  //for a parallel approach. If we break into chunks the input and count separately, what is the problem? For the
	  //first and last words of the segment we cannot tell whether they are single words or halves. We define:
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: Stub, words: Int, rStub: Stub) extends WC
  //where Stub represents the state in which we have read some chars (param) but not any full word yet; Part counts
  //the number of full words and keeps the strings for the first and last "a cavallo" words.
  //A monoid for it is:
  def wcMonoid = new Monoid[WC] {
    private val stringMonoid = Monoid.stringMonoid 
    private val stringZero = stringMonoid.zero
    private val stringPlus = stringMonoid.+_
    def zero = Stub(stringZero)
    //Assumption: ' ' is the only space character and the text never contains two or more spaces one after the other. 
    def +(wc1: WC, wc2: WC) = (wc1, wc2) match {
      case (Stub(s1), Stub(s2)) => Stub(stringPlus(s1, s2))
      case (Stub(s1), Part(Stub(s2), c, rs)) => Part(Stub(stringPlus(s1, s2)), c, rs)
      case (Part(ls, c, Stub(s1)), Stub(s2)) => Part(ls, c, Stub(stringPlus(s1, s2)))
      case (Part(ls1, c1, _), Part(_, c2, rs2)) => Part(ls1, c1+c2+1, rs2)
    }
  }
  def wc(text: String, chunkSize: Int): WC = {
    def zero = wcMonoid.zero
    def go(text: CharSequence, wc: WC): WC = {
      val textLength = text.length
      if (textLength < 1) wc
      else {
        val char = text.charAt(0)
        val nextText = text.subSequence(1, textLength)
        wc match {
          case Stub(charz) if char != ' ' => go(nextText, Stub(charz+char))
          case s: Stub if char == ' ' => go(nextText, Part(s, 0, zero))
          case p@Part(_, _, Stub(charz)) if char != ' ' => go(nextText, p.copy(rStub = Stub(charz+char)))
          case p: Part if char == ' ' => go(nextText, p.copy(words = p.words+1, rStub = zero))
        }
      }
    }
    val textSize = text.size
    if (textSize > chunkSize) {
      val (t1, t2) = text.splitAt(textSize/2)
      wcMonoid.+(wc(t1, chunkSize), wc(t2, chunkSize))
    }
    else go(text, zero)
  }
  
  //Let M and N be two monoids and h: M => N; h is a homomorphism for M if the following holds:
  //h(id_M)=id_N
  //M.+(h(x), h(y)) == h(N.+(x, y))
  //For example:
  val N = Monoid.stringMonoid
  val M = Monoid.intSumMonoid
  val h: String => Int = _.length
  def homoLaw(a: String, b: String) = assert(M.+(h(a), h(b)) == h(N.+(a, b)))
  //If the inverse holds too then h is an isomorphism. In this case we say that the monoid structure is preserved in
  //meaning that Im(h)=Int and Int is still a monoid.
  
  //The fold operation is so ubiquitous that it deserves its own abstraction. Foldable cannot extend Functor in general
  //but it is doable for some instances, eg List; as a naive argument: pick foldMap, and let B = F[C], now you have
  //many F[C]'s that you want to combine in a single F[C] at the same time keeping the distinct values; you can think
  //of a counterexample (eg some kind of list where the sum is the intersection or so, I think the monoid laws still
  //hold).
  trait Foldable[F[_]] {
	  def foldRight[A, B](as: F[A])(mb: Monoid[B])(f: (A, B) => B): B
	  def foldMap[A, B](as: F[A])(mb: Monoid[B])(f: A => B): B
	  def concatenate[A](as: F[A])(ma: Monoid[A]): A
	  //EX93 2list
	  def toList[A](fa: F[A]): List[A] = foldRight[A, List[A]](fa)(Monoid.listMonoid)((a, as) => a :: as)
	  //EX94 2baggie
	  def toBag[A](fa: F[A]): SMap[A, Int] = foldRight[A, SMap[A, Int]](fa)(Monoid.mapMergeMonoid(Monoid.intSumMonoid))((a, bag) => bag + ((a, bag.get(a).map(_+1).getOrElse(1))))
  }
  
  //EX 88 TreeFoldable
  trait TreeFoldable extends Foldable[Tree] {
    def foldRight[A, B](t: Tree[A])(mb: Monoid[B])(f: (A, B) => B): B = t match {
      case EmptyTree => mb.zero
      case Node(a, l, r) => f(a, mb.+(foldRight(l)(mb)(f), foldRight(r)(mb)(f)))
    }
    def foldMap[A, B](t: Tree[A])(mb: Monoid[B])(f: A => B): B = t match {
      case EmptyTree => mb.zero
      case Node(a, l, r) => mb.+(f(a), mb.+(foldMap(l)(mb)(f), foldMap(r)(mb)(f)))
    }
	  def concatenate[A](t: Tree[A])(ma: Monoid[A]): A = t match {
	    case EmptyTree => ma.zero
	    case Node(a, l, r) => ma.+(a, ma.+(concatenate(l)(ma), concatenate(r)(ma)))
	  }
  }
  
  //Finally, the idea of "something that has a map" (and other handy operations that come with it).
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = ??? //this is the only primary operator we need; ??? to allow super's
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2)) //this is basically an unzip
    object Laws {
      def id[A](fa: F[A]) = assert(map(fa)(identity) == fa)
	    def assoc[A, B, C](fa: F[A], f: A => B, g: B => C) = assert(map(map(fa)(f))(g) == map(fa)(f andThen g))
    }
  }
  
  //The concept of functor lives on more abstract floors than the one of monoid, the concept of set is not enough and
  //we need to introduce that of category.
  //For a given structure C a category is a collection of objects ob(C) together with a class of morphisms (maps)
  //hom(C) between these objects; hom(C) can be partitioned in classes of maps for a given pair of objects, ie all of
  //the maps from object A to object B. Morphisms must be equipped with a composition operation o allowing us to link
  //them. For this triple to be a category there must be an identity morphism for every object and the morphism 
  //composition must be associative. It is also legit to see a monoid as a category whose only object is the support
  //set. A category is referred to as small if both ob and and hom are sets rather than classes, ie collections of 
  //sets. The number of types in Scala can be informally seen as a category, where every different type is an object. 
  //Given two categories C and D a functor F is a mapping F: C -> D such that:
  //forall X in ob(C) F(C) is in ob(D)
  //forall f in hom(C) F(f)=F(dom_f)->F(cod_f) is in hom(C)
  //and the following conditions hold:
  //forall id_X in hom(C) F(id_X)=id_F(X) (identity preservation)
  //forall f, g in hom(C) F(f o g)=F(f) o F(g) (composition preservation)
  //In our Scala category C and D coincide of course, and let's take List as the functor F. On a given type A from
  //ob(Scala), List[A] is still in ob(Scala), and for another arbitrary type B any f: A => B in hom(Scala) we have
  //g: List[A]->List[B] exactly as above thanks to the map operation.
  //A contravariant functor reverses the composition order for the second condition.
  
  //Another important abstraction is that of a monad. On the formal category-theory level it is defined as following:
  //for the category C a monad on it is a triple M=(T:C->C, h: 1_C->T, m: T^2->T), where 1_C is the identity functor
  //on C (Id in scalaz) and T^2 is the functor T o T. The two following conditions must hold:
  //m o Tm = m o mT
  //m o Th = m o hT = 1_T
  //where the form Xx is a functor-to-functor map (natural transformation), whose details are really too troublesome
  //for this discussion.
  //The map is there since a monad is a functor; unit, flatMap and map2 roughly embody the maps from the triple
  //definition. The concept has close resemblance to that of a monoid; infact a monad can be seen as a monoid on the
  //category End_C whose objects are endofunctors in C and morphisms the natural transformations between them.
  trait Monad[F[_]] extends Applicative[F] {
    //primitive combinators - ??? so that we can skip implementations when clear
	  def unit[A](a: => A): F[A] = ???
	  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = ???
    //other
	  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
	  override def product[A, B, C](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
	  //EX 95 traversin and sequencin
	  override def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldRight(unit(List[B]())) { (a, fbs: F[List[B]]) => map2(f(a), fbs)(_ :: _) }
    override def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)
	  //EX 96 replication
	  override def replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
	    if (n <= 0) unit(List[A]())
	    else map2(ma, replicateM(n-1, ma))(_ :: _)
	  //EX 97 Monadic filter
	  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = map(
	    sequence(ms.map(a => map2(unit(a), f(a))((a, fa) => if (fa) List(a) else List())))
	  )(_.flatten)
	  //EX 98 composition through Kleisli functions
	  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = f.andThen(fb => flatMap(fb)(g))
	  //EX 99 the other way around (assuming some other implementation for compose of course; HAF, hard as fuck)
	  def lift[A, B](f: A => F[B]): F[A] => F[B] = compose[F[A], A, B](identity, f)	  
	  def unlift[A, B](f: F[A] => F[B]): A => F[B] = compose[A, F[A], B](a => unit(unit(a)), f)
	  def composeFlatmap[A, B](ma: F[A])(f: A => F[B]): F[B] = lift(f)(ma)
	  //EX 100 (cento, cento, cento!) join (tanto per rimanere in tema lavoro); questo è in pratica il flatten, inverso
	  //di unit
	  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
	  //EX 101 "Implement either flatMap or compose in terms of join"
	  def joinCompose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
	    val ffc: A => F[F[C]] = a  => map(f(a))(g)
	    ffc.andThen(join)
	  }
    //Contrib:
    def skip[A](a: F[A]): F[Unit] = map(a)(_ => ())
    //Repeats the effect of the first argument as long as the cond function yields true.
    def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = flatMap(flatMap(a)(cond))(x => if(x) doWhile(a)(cond) else unit())
    //Repeats the effect of its argument infinitely. Watch out, @tailrecness depends on the flatMap implementation!
    def forever[A, B](a: F[A]): F[B] = {
      lazy val t: F[B] = forever(a)
      flatMap(a)(_ => t)
    }
    //Folds the stream with the function f, combining the effects and returning the result.
    //We use scala's stream as for ours we didn't implement the extractor
    def foldM[A, B](l: SStream[A])(z: B)(f: (B, A) => F[B]): F[B] = l match {
      case h #:: t => flatMap(f(z, h))(z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }
    //The same as the foldM function above except ignores the result. 
    def foldM_[A, B](l: SStream[A])(z: B)(f: (B, A) => F[B]): F[Unit] = skip { foldM(l)(z)(f) }
    //Calls the function f for each element of the stream and combines the effects.
    def foreachM[A](l: SStream[A])(f: A => F[Unit]): F[Unit] = foldM_(l)(())((u,a) => skip(f(a)))
    
	  //Monads, in general, do not compose: as they are applicatives I can but only the applicative behavior is therefore
    //retained. If one of the two monads is also a traversable then full composition is possible. A pattern used in
    //this sense is the monad transformer: for every traversable monad I would like to compose I define a class
    //which composes with an arbitrary monad.
    case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
      //This travels both on M and Option and allows M[Option[M[Option[A]]]] to just M[Option[A]]
      def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = OptionT(M.flatMap(value) {
        case None => M.unit(None)
        case Some(a) => f(a).value
      }) 
    }
	  trait FunctorLaws { 
	    import Laws.{id => functorId, assoc => assocId} 
	    def unitLaw[A](a: A): F[A] = ??? //this is for the definition of the monadic laws below, it is strict
	  }
	  object Laws1 extends FunctorLaws {
      def rightId[A](fa: F[A]) = assert(flatMap(fa)(unitLaw) == fa)
      def leftId[A, B](a: A, f: A => F[B]) = assert(flatMap(unit(a))(f) == f(a))
      def assoc[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]) = assert(flatMap(flatMap(fa)(f))(g) == flatMap(fa)(a => flatMap(f(a))(g)))
    }
	  object Laws2 extends FunctorLaws {
	    def rightId[A, B](f: A => F[B]) = assert(compose(f, unitLaw[B]) == f)
	    def leftId[A, B](f: A => F[B]) = assert(compose(unitLaw[A], f) == f)
	    def assoc[A, B, C, D](f: A => F[B], g: B => F[C], h: C => F[D]) = assert(compose(compose(f, g), h) == compose(f, compose(g, h)))
	  }
	  //The minimal functionally-complete sets of primitives - a trait must have at least a superset of any of these to
	  //be called a monad:
	  //1 unit & flatMap
	  //2 unit & compose
	  //3 unit, map, join - since with map and join i can build flatMap or compose
	  //So cutting the chase the the functional facts, a monad is an abstraction following this rule, with the sets of
	  //combinators respecting the identity and associativity laws (see above)
  }
  
  //EX 102 - Implement the Id monad. Now in real world Scala, since the type system is powerful yet not that powerful,
  //it is hardly every possible to both represent the above abstractions neatly and at the same time conform to the
  //style etiquette, for example the synergy with companion objects. In the following we will implement all of the
  //monadic operations yet without explicitly referring to the abstractions depicted above.
  
  case class Id[A](a: A) {
    def unit(a: A) = Id(a)
    def flatMap[B](f: A => Id[B]): Id[B] = f(a) 
  }
  
  //Recall the State abstraction above:
  //type State[S, +A] = S => (A, S)
  //Now we implemented map and flatMap for it, but see State has two type parameters. The situation here is analogous
  //to what we have with a partial function: fixed the S parameter we want to be able to operate with an instance no
  //matter the particular color A. We can have something like:
  object IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState]
  //What we are doing here it is simply creating an anonymous object with a type member which we access with the #-
  //notation. This is really like:
  trait LS { type LongState[A] = State[Long, A] }
  object LongStateMonad extends Monad[LS#LongState]
  //and we can also do:
  type StringState[A] = State[String, A]
  object StringStateMonad extends Monad[StringState]
  //which is the most readable option probably.
  //An inline-defined type as above is often referred to as type-lambda since it is an anonymous type.
  //This leads us to a State monad factory as the following:
  object Monad {
    def stateMonad[S] = new Monad[({type T[A] = StateClass[S, A]})#T] {
      override def unit[A](a: => A): StateClass[S, A] = StateClass(s => (a, s))
      override def flatMap[A, B](st: StateClass[S, A])(f: A => StateClass[S, B]): StateClass[S, B] = st flatMap f
    }
    def idMonad[A]: Monad[Id] = new Monad[Id] {
      def unit[A](a: A) = Id(a)
      override def flatMap[A, B](id: Id[A])(f: A => Id[B]): Id[B] = f(id.a) 
    }
  }
  //A for cycle is just an alias for a combination of map, flatMap and filter. A monad can therefore also be seen as
  //the specification of what occurs at statement boundaries in a comprehension. With the Id-entity monad nothing
  //happens, just (un)wrapping; with Option instead for example the result is wrapped in an Option subclass: if Some
  //the execution continues, if None it is stopped and none of the following operations applied.
  
  //EX 103 - the reader monad. The fondamental operation is to read a value of course. A flatMap allows to transform
  //a reader on input R of values of type A into one of type B. Sequence of course gives you the possibility to read
  //a list of a's if you only have n readers for getting different a's.
  case class Reader[R, A](run: R => A) {
    def read(r: R): A = run(r)
  }
  object Reader {
	  def readerMonad[R] = new Monad[({type T[A] = Reader[R, A]})#T] {
	    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
	    override def map[A, B](st: Reader[R, A])(f: A => B): Reader[R, B] = st.copy(run = st.run.andThen(f))
	    override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader{r => 
	      val a = st.run(r)
	      f(a).run(r)
	    }
	  }
  }
  
  //EX 106 - either monad (see above)
  
  //Our next abstraction on the list is the applicative functor. In terms of expressive power, it sits in between the
  //functor and the monad, and it does not come from a precise category theory abstraction.
  trait Applicative[F[_]] extends Functor[F] { self =>
	  // primitive combinators
	  def unit[A](a: => A): F[A]
	  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    // derived combinators
    override def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
	  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
	  //EX 104 Some other derived combinators. Copy-paste exercise, the definition are identical to those in Monad!
	  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
	  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
	    if (n <= 0) unit(List[A]())
	    else map2(ma, replicateM(n-1, ma))(_ :: _)
    //"internal" product
	  def product[A, B, C](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
    //generic product - this is what we implemented in the companion, for Monoid; notice that applicatives also work
    //on "composition", ie F[G] is an applicative too
    //EX XYZ
    def product[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f] {   
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      def map2[A, B, C](fga: (F[A], G[A]), fgb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (self.map2(fga._1, fgb._1)(f), G.map2(fga._2, fgb._2)(f))
    }
    //Also, applicative compose naturally.
	  
	  trait FunctorLaws { 
	    import Laws.{id => functorId, assoc => assocId} 
	    def unitLaw[A](a: A): F[A] = ??? //this is for the definition of the monadic laws below, it is strict
	  }
	  object Lawz extends FunctorLaws {
      def rightId[A](a: A, fa: F[A]) = assert(map2(unit(()), fa)((_,a) => a) == fa)
      def leftId[A](a: A, fa: F[A]) = assert(map2(fa, unit(()))((a,_) => a) == fa)
      
      def assoc[A, B, C](fa: F[A], fb: F[B], fc: F[C]) = assert(product(product(fa,fb), fc) == map(product(fa, product(fb,fc)))(ass))
      private def ass[A, B, C](p: (A,(B, C))): ((A, B), C) = p match { case (a, (b, c)) => ((a,b), c) }
      
      def productF[I, O, I2, O2](f: I => O, g: I2 => O2): (I, I2) => (O, O2) = (i,i2) => (f(i), g(i2))
      def natur[A, B, C](fa: F[A], fb: F[B], f: A => B, g: B => C) = assert(map2(fa, fb)(productF(f, g)) == product(map(fa)(f), map(fb)(g)))
	  }
  }
  
  //Any monoid can be transformed into an applicative:
  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) =
	  new Applicative[({ type T[A] = Const[M, A] })#T] {
	    def unit[A](a: => A): M = M.zero
	    def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.+(m1, m2)
	  }
  
  //So we have unit, as monad has, but map2 rather than flatMap; with map2 we are also implementing map so that the
  //contract from Functor is respected.
  //Why the name "applicative"? Actually because replacing map2 with apply leads to an equivalent structure.
  //EX 105
  trait Applycative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)
  }
  //Of course all monads are applicative functors. But when to use either? We can see that there is quite some algebra
  //difference by looking at the method signatures, yet we can also say that applicative computations can only 
  //sequence effects, whereas monadic can allow for flow control according to the different results. Applicative is
  //context-free, monadic context-sensitive. Also, applicative functors compose, monads don't (more on this later).
  //Example, form validation: we are better off with a monad since after the first error we can skip the following
  //inputs altogether. In any case, hey we need some extended Either to handle more than one error:
  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]
  //and we can even make it an applicative:
  object Validation {
    def validationMonad[E] = new Monad[({type T[A] = Validation[E, A]})#T] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)
      override def flatMap[A, B](v: Validation[E, A])(f: A => Validation[E, B]): Validation[E, B] = v match {
        case Success(a) => f(a)
        case f: Failure[_] => f
      }
      def flatMapAlt[A](v: Validation[E, A])(f: E => Validation[E, A]): Validation[E, A] = v match {
        case s: Success[_] => s
        case Failure(e, es) => f(e) match {
            case s: Success[_] => s
            case Failure(h, hs) => Failure(h, hs ++ (es.+:(e)))
        } 
      }
    }
  }
  
  //With sequence and traverse above, we see a particular type, List, occurring in some abstract interfaces. Whenever
  //this happens then we should think of abstracting up. Let us therefore define:
  trait Traversable[T[_]] extends Functor[T] with Foldable[T] {
    //primary and only operation, ready for us thanks to Functor!
    def traverse[F[_] : Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] = sequence(super.map(ta)(f))
    //other
    def sequence[F[_] : Applicative, A](tfa: T[F[A]]): F[T[A]] = traverse(tfa)(identity)
    //EX 106 Traversable is more powerful than functor; notice how we are using an Id "a la minute"
    val idMonad = new Applicative[Id] {
      def unit[A](a: => A) = Id(a)
      def map2[A, B, C](id1: Id[A], id2: Id[B])(f: (A, B) => C): Id[C] = unit(f(id1.a, id2.a))
    }
    implicit def fromId[A](id: Id[A]) = id.a
    override def map[A, B](ta: T[A])(f: A => B): T[B] = traverse(ta)(f.andThen(Id(_)))(idMonad)
    def foldMap[A, M](as: T[A])(f: A => M)(mb: Monoid[M]): M = traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))
    
    //An interesting application for this abstraction is traversing a structure by accumulating some state:
    def traverseS[S, A, B](ta: T[A])(f: A => StateClass[S, B]): StateClass[S, T[B]] = traverse[({type f[x] = StateClass[S, x]})#f, A, B](ta)(f)(Monad.stateMonad)
    //The generic pattern is:
    def mapAccum[S, A, B](ta: T[A], s: S)(f: (A, S) => (B, S)): (T[B], S) = traverseS(ta)((a: A) => (for {
      s1 <- StateClass.get[S]
      (b, s2) = f(a, s1)
      _  <- StateClass.set[S](s1)
    } yield b)).r(s)
    //An example is zipping a collection with its element indexes:
    def zipWithIndex[A](ta: T[A]): T[(A, Int)] = mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1
    //and another is converting to a list:
    override def toList[A](ta: T[A]): List[A] = mapAccum(ta, List[A]())((a, s) => ((), a :: s))._2.reverse
    //EX 107 - a state traversal for reversing traversables
    def reverse[A](ta: T[A]): T[A] = mapAccum(ta, toList(ta).reverse)((_, as) => (as.head, as.tail))._1
    //EX 108 - satisfy Foldable's foldRight via mapAccum
    def foldRight[A, B](as: T[A])(mb: Monoid[B])(f: (A, B) => B): B = mapAccum(as, mb.zero)((a, s) => ((), f(a, s)))._2
    //Basic zip - the two input T's must share the same structure, for example the length is the same, if T = List
    def zip[A, B](ta: T[A], tb: T[B]): T[(A, B)] = (mapAccum(ta, toList(tb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    })._1
    //Left-biased version: the outcome is dictated by the first input
    def zipL[A, B](ta: T[A], tb: T[B]): T[(A, Option[B])] = (mapAccum[List[B], A, (A, Option[B])](ta, toList(tb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1
    //EX 109 - traversal fusion: we want to go down and collect for both functions but only once
    def fuse[G[_], H[_], A, B](ta: T[A])(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[T[B]], H[T[B]]) =
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](ta)(a => (f(a), g(a)))(G product H) //this syntax is unbelievable
    //Anyway, traversable composes too, we can get a single traversable from two different ones without touching
    //other structures
    
  }
  //and voila we have the traversable functor! Moreover this is an extension too since exactly as for Applicative we
  //can reimplement map in terms of traverse (see above). Illumination: traversable : applicative = foldable : monoid


  //What we have learned so far can be applied also to strictly effectful programs, those that have side effects, 
  //where a good examples are screen output and database persistency. First of all, inside every function with side 
  //effects there is a pure function waiting to get out. Given any impure function f: A => B it can be split into
  //two functions g: A => C, h: C => B where g is a pure function and C is the type of the input for h, the
  //description, for example a string message, and h only is impure.

  trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
  }
  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }

  //Example:
  def ReedLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  def converter(f: Double => Double): IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReedLine.map(_.toDouble)
    _ <- PrintLine(f(d).toString)
  } yield ()
  //So we assemble a computation abstraction which does not have any side effect at its creation, it has only if its
  //run method is invoked. 
  //Pros:
  //-IO computations are ordinary values, we can therefore pass them around, map them etc
  //-different run methods can be localized for different types and the interface for the user will not change
  //Cons:
  //-IO[A] is opaque in the sense that it does not offer ways for us to inspect it, it is basically an identity
  //-so fare we are completely synchronous, see below for an improvement in this sense
  //-a program on a repl-like implementation as the above may, eventually, stack overflow
  
  //About the overflow, consider
  val sg = IO.forever(PrintLine("Still going..."))
  //this will fill up the stack, as the way flatMap is more and more nested IO's will be created. Solution ("simple"):
  //defining the control for the flow, not only the flow itself. In this picture when we execute flatMap we are not
  //actually computing the step, but rather returning a container for what we were returning before, symbolically.
  //Let's therefore rewrite our IO trait as:
  //aka TailRec[A]
  sealed trait NIO[A] {
    def aBetterFlatMap[B](f: A => NIO[B]) = this match {
      case FlatMap(x, g) => FlatMap(x, (a: Any) => g(a).flatMap(f))
      case x => FlatMap(x, f)
    }
    def flatMap[B](f: A => NIO[B]): NIO[B] = FlatMap(this, f)
    def map[B](f: A => B): NIO[B] = FlatMap(this, (a: A) => Return(f(a)))
  }
  object NIO extends Monad[NIO] {
    override def unit[A](a: => A): NIO[A] = new NIO[A] { def run = a }
    override def flatMap[A, B](fa: NIO[A])(f: A => NIO[B]) = fa flatMap f
    def apply[A](a: => A): NIO[A] = unit(a)
    @annotation.tailrec 
    def run[A](io: NIO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => run(r())
      case FlatMap(x, f) => x match { //pattern match so we are tailrec!
          case Return(a) => run(f(a))
          case Suspend(r) => run(FlatMap(r(), f))
          //this rewriting is possible thanks to the monad associative law(s): FlatMap(FlatMap(y,g),f) is the same as 
          //FlatMap(y, a => FlatMap(g(a),f))
          case FlatMap(y, g) => run(FlatMap(y, (a: Any) => FlatMap(g(a), f))) 
      }
    }
    @annotation.tailrec 
    def aBetterRun[A](io: NIO[A]): A = io match {
      case Return(a) => a
      case Suspend(r) => aBetterRun(r())
      case FlatMap(x, f) => x match {
        case Return(a) => aBetterRun(f(a))
        case Suspend(r) => aBetterRun(r() flatMap f)
        case FlatMap(y, g) => aBetterRun(y flatMap (a => g(a) flatMap f))
      }
    }
    //This control flow technique is called trampolining: this is some somewhat emulating what the execution of a
    //recursive program is on the JVM and almost everywhere else, at each call we frame up the stack and the memory
    //pointer let us safely return the control to the caller. Here, the run method, or co-routine if you will, does
    //basically the same but we don't go up the stack indefinitely, the control goes back to run immediately, with
    //one of the three control flavors R, S, F.
  }
  case class Return[A](a: A) extends NIO[A] //unit
  case class Suspend[A](resume: () => NIO[A]) extends NIO[A] //resume for example Thread.sleeps for a while
  case class FlatMap[A, B](sub: NIO[A], k: A => NIO[B]) extends NIO[B] //first go down with sub, when we get an a, k(a)
  //Where:
  //-Return is a simple computation which terminates as created, it is the stopping point for a run
  //-Suspend is used to perform some side effect and start again an interrupted computation, with the desired input
  //-FlatMap: when processed, its sub IO should be executed first, this is possibly another flatMap; when a Return is
  // hit, k is applied to the IO[A] from it; k can be seen as a continuation
  //This is a DSL for the control flow of the run method, which turns the IO program run executes into a number of 
  //nested case classes.
  //Thus we can have:
  def NPrintLine(s: String): NIO[Unit] = Suspend(() => Return(println(s)))
  val p = NIO.forever(NPrintLine("Still going..."))
  //Of course, this is still an infinite structure, some sort of stream, where Return is the empty stream and FlatMap
  //acts as concatenation, but thanks to the tailrecness of the run method above we can go on and on
 
  //But, helaas, here is another stack-problematic program:
  val actions: SStream[NIO[Unit]] = SStream.fill(1000000)(NPrintLine("Still going I hoooooooope..."))
  val composite: NIO[Unit] = actions.foldLeft[NIO[Unit]](Return(())) { (acc, a) => acc flatMap { _ => a } }
  //If we do run(composite), we will stack overflow because of the way FlatMaps can happen to be chained: if we have
  //a left-leaning tower, ie FlatMap(FlatMap(FlatMap..., and it is infinite, then we never stop going down on it. A
  //remedy is the aBetterFlatMap above, which forces right-nesting, and requires changes in the run too, see aBetterRun.
  
  //What we do is to exchange stack for heap. Instead of making a tail call, we return a data structure representing
  //what to do next.
  
  //We evolved quite fast here, and we can also say that the trampolining style is applicable to a general computation
  //of course, no need for it to do IO. Let's have a look at such example:
  val func = (x: Int) => x
  val gunc = List.fill(100000)(func).foldLeft(func)(_ compose _)
  gunc(42)
  //func is the id, we create a list of N func's and we compose them func(func(func(...; it SOEs
  //Let's use our NIO:
  val funcio: Int => NIO[Int] = (x: Int) => Return(x)
  val guncio = List.fill(100000)(funcio).foldLeft(funcio) {(a, b) => x => Suspend(() => a(x).flatMap(b))}
  NIO.aBetterRun(guncio(0))
  NIO.aBetterRun(guncio(42))
  val xio = NIO.aBetterRun(guncio(42))
  //Therefore, more generally trampolining can be seen as a way to make safe recursion. We can transform any f: A => B
  //into a  f': A => NIO[B], where a good name for NIO is also TailRec. We will see a related concept, that of Kleisli
  //composition.
  
  //Problems we still have with this abstraction:
  //-the idea of (side) effect is not explicit
  //-the IO side effect is performed on the main thread, yielding therefore to possible blocks
  
  //About the first point, as we hinted to the question is that an effect s in Suspend(s) is quite opaque, it is
  //just a function which does something and returns a state for the computation to go on, one of the three states.
  //And being s a simple function, all we can do is call it, then sit and wait. We want to have something more generic
  //than a function, conveniently, to have more control options. So TailRec uses f: A => B whereas Free uses a container
  //F parametrized in Free itself, which is our new abstraction trait type:
  
  //EX 110: a Free monad generator; we use again the lambda trick because monad accepts a higher kind of arity 1 but
  //our Free here is arity 2. Again there is a little confusion about where to put what. For usual purposes F is a
  //functor
  
  sealed trait Free[F[_], A] { //For F=Function0 what we get is the TailRec above; you see it clearly in SuspendFree
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMapFree(this, f)
    def map[B](f: A => B): Free[F, B] = FlatMapFree(this, (a: A) => ReturnFree(f(a))) 
  }
  case class ReturnFree[F[_], A](a: A) extends Free[F, A]
  case class SuspendFree[F[_], A](s: F[Free[F, A]]) extends Free[F, A]
  case class FlatMapFree[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  object Free {
    def freeMonad[F[_]] = new Monad[({type f[a] = Free[F, a]})#f] {
      def apply[A](a: => A): Free[F, A] = unit(a)
      override def unit[A](a: => A): Free[F, A] = ReturnFree(a) 
      override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f 
    }
  }
  
  //With freeMonad we can put together a structure with a value of type A buried under many F-type layers. The run
  //interacts with the structure according to the nature of F. We cannot really generalize the execution method, this
  //is doable in a meaningful way for a particular F only, as we did above for the container being a constant. Let's
  //look at an example on a different F:
  sealed trait Console[R] //R stands for the rest of the computation to be interleaved between inputs and outputs
  case class ReadLine[R](k: Option[String] => R) extends Console[R] //read the value if any and produce the next state
  case class PrintLine[R](s: String, k: () => R) extends Console[R] //print the value and move on with the state
  object Console {
    type ConsoleIO[A] = Free[Console, A] // when we read, our A is a String, when we print there is none
    //And now the logic:
    def readLn: ConsoleIO[Option[String]] = SuspendFree(ReadLine((s: Option[String]) => ReturnFree(s)))
    //the io suspension is reading the string, and the after that we can simply return it 
    def printLn(s: String): ConsoleIO[Unit] = SuspendFree(PrintLine(s, () => ReturnFree(())))
    //here it is to print it instead, and after this we don't have anything to return
    //EX 111 - the console interpreter. A debug interpreter could instead always print and read default values - it is
    //the responsability of the interpreter to "honor" the commands!
    def runConsole[A](io: ConsoleIO[A]): A = io match { 
      case ReturnFree(a) => a
      case SuspendFree(ReadLine(k)) => runConsole(k(Option(readLine)))
      case SuspendFree(PrintLine(s, k)) => println(s); runConsole(k())
      case FlatMapFree(x, f) => x match {
        case ReturnFree(a) => runConsole(f(a))
        case SuspendFree(ReadLine(s)) => runConsole(s(Option(readLine)) flatMap f)
        case SuspendFree(PrintLine(s, k)) => println(s); runConsole(k() flatMap f)
        case FlatMapFree(y, g) => runConsole(y flatMap (a => g(a) flatMap f))
      }
    }
    def toId[R](c: Console[R]): Id[R] = c match {
      case ReadLine(k) => Id(k(Option(readLine)))
      case PrintLine(s, k) => println(s); Id(k())
    }

  }
  //Example:
  import Console.{printLn, readLn}
  val f1: Free[Console, Option[String]] = for {
    _  <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln
  //which is equivalent to:
  val f1eq: Free[Console, Option[String]] = SuspendFree(PrintLine("I can only interact with the console.", () => SuspendFree(ReadLine(ln => ReturnFree(ln)))))

  //Both the interpreters above share a common recursion structure, which we can therefore generalize. First of all,
  //Free is a monad, input to the run method which returns a value some type, so we can also see run as a monadic
  //map from Free to Id. To proceed from this fact, we need a transformation of the form:
  //def apply[A](f: F[A]): G[A]
  //If we can transform all of the containers F into G, then we can of course collapse them with the join operation
  //and get to have a single G; infact let's not forget that Free[F, A] is the wrapping of some a in A with many
  //instances of type F. The map above is called natural transformation.
  //We need to introduce a new abstraction, since Function's may not have free type parameters:
  trait ~>[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }
  //A first sketch for our executor then becomes:
  def runFree[F[_], G[_], A](free: Free[F, A])(f: F ~> G)(implicit G: Monad[G]): G[A] = free match {
    case ReturnFree(a) => G.unit(a)
    case SuspendFree(s) => G.join(G.map(f(s))(x => runFree(x)(f)(G)))
    case FlatMapFree(x, h) => x match {
      case ReturnFree(a) => runFree(h(a))(f)(G)
      case SuspendFree(s) => runFree(h(s) flatMap h)(f)(G)
      case FlatMapFree(y, z) => runFree(y flatMap (a => z(a) flatMap h))(f)(G)
    }
  }
  //EX 112; the runConsole from above thus becomes:
  import Console.{ConsoleIO, toId}
  object ConsoleIdNatural extends ~>[Console, Id] {
    def apply[A](c: Console[A]): Id[A] = toId(c)
  } 
  def runFreeConsole[A](io: ConsoleIO[A]): A = runFree(io)(ConsoleIdNatural)(Monad.idMonad).a
  //The amazing thing here is that we started off to design an abstraction for clean side effecting and evolved to a
  //pretty generic, and thus powerful, one, where the side effect is something completely isolated. Other meaningful
  //monads for G are State and Reader. And most powerfully: if we want to get nonblocking, we use Future rather than 
  //Id! But if we still use println and readLn we will just block somewhere else :( We can imagine a callback-based
  //library instead, then we could get asynchronous as well, so in the same way as above let's move on to define a 
  //container for Free:
  sealed trait RWBytes[A]
  case class ReadBytes[A](n: Int, k: Either[Throwable, Array[Byte]] => A) extends RWBytes[A]
  case class WriteBytes[A](buf: Array[Byte], onError: Throwable => A, k: () => A) extends RWBytes[A]
  
  type ByteIO[A] = Free[RWBytes, A]
  object ByteOps {
    def readBytes(n: Int): ByteIO[Either[Throwable, Array[Byte]]] =
      SuspendFree(ReadBytes(n, response => ReturnFree(response)))
    def writeBytes(buf: Array[Byte]): ByteIO[Option[Throwable]] =
      SuspendFree(WriteBytes(buf, t => ReturnFree(Some(t)), () => ReturnFree(None)))
  }
  
  //Example: source-sink continuous pipe
  import ByteOps._
  def pipe(bufferSize: Int): ByteIO[Throwable] = readBytes(bufferSize) flatMap {
    case Left(e) => ReturnFree(e)
    case Right(bs) => for {
      _ <- writeBytes(bs)
      e <- pipe(bufferSize)
    } yield e
  }
  
  //A contract it is fair to expect from a nonblocking reader implementation; it returns immediately, the callback
  //will be fed when the input is ready
  trait Surs {
    def readBytes(nBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
  }
  //and for a wirter:
  trait Synk {
    def writeBytes(buf: Array[Byte], onError: Throwable => Unit): Unit
  }
  
  //Let's use Parallel as our target monad:
  def rwBytesToPar(sink: Synk, source: Surs) = new (RWBytes ~> Parallel) {
    def apply[A](bs: RWBytes[A]): Parallel[A] = bs match {
      case ReadBytes(n, k) => Parallel.async { h =>
        source.readBytes(n, e => h(k(e)))
      }
      case WriteBytes(buf, err, k) => Parallel.async { h =>
        sink.writeBytes(buf, t => h(err(t)))
        k() 
      }
    } 
  }
  
  //We are now ready for a general model of IO operativity. First of all we decide about the kind of interface we
  //need (eg console, database, file...). Then, we collect the operations we want to support (eg for a console it is
  //read and write a line) and we define a case class for each of them; the case classes share a common parent, which
  //is the argument F, the F we instance the Free monad with. We can also choose what the output wrap will be, the
  //pure value or some nonblocking container maybe, like a Future. The different IO machineries can be tested
  //separately, but still on the very same generic-purpose runner. Such IO abstraction may be reduced to a common one
  type Io[A] = Free[Parallel, A]
  //What this gives us is trampolined asyncronous execution, and the transformation can of course be made automatic
  //via implicits if the natural map F ~> Parallel is in scope.

  //In the Free above we said that F is a functor, and that a Free monad is always possible, eg for an arbitrary
  //functor F there exists. But also, for every constructor F there exists a free functor:
  sealed trait FreeFunctor[F[_], A] {
    def map[B](f: A => B): FreeFunctor[F, B]
  }
  case class Map[F[_], I, A](fa: F[I], g: I => A) extends FreeFunctor[F, A] {
    def map[B](f: A => B) = Map(fa, g andThen f)
  }
  //and we can even automate things a little:
  type FreeC[F[_], A] = Free[({type f[x] = FreeFunctor[F,x]})#f, A]
  implicit def request[F[_], A](fa: F[A]): FreeC[F, A] = 
    SuspendFree[({type f[x] = FreeFunctor[F, x]})#f, A](Map(fa, (a: A) =>
      ReturnFree[({type f[x] = FreeFunctor[F, x]})#f, A](a)))

  //This let's us write the functor without worrying about "the rest" of the computation
  sealed trait FreeConsole[A]
  case object FreeReadLine extends FreeConsole[Option[String]]
  case class FreePrintLine(s: String) extends FreeConsole[Unit]
  
  //because we already get the "full" Console above implicitly:  
  val program: FreeC[FreeConsole, Unit] = for {
    _    <- FreePrintLine("What is your name?")
    name <- FreeReadLine
    _    <- name map { n => FreePrintLine(s"Hello, $n!") } getOrElse FreePrintLine("Fine, be that way.")
  } yield ()
  
  //Now this program is still a Free, just FreeC is the alias for a Free constructed from a FreeFunctor, therefore we
  //can just pass it to runFree for execution, which also requires a natural transformation from the functor F of the
  //Free to a monad of choice, therefore we have to provide a FreeFunctor[H, A] => G because here F = FreeFunctor.
  //Now if we already have a map H => G, and G is at least a functor, we can lift it:
  //EX 113 DIFFICILISSIMO ?!? Noooo:
  def freeLift[F[_], G[_]](fg: F ~> G)(implicit G: Functor[G]): ~>[({type f[x]=FreeFunctor[F, x]})#f, G] = new ~>[({type f[x]=FreeFunctor[F, x]})#f, G] {
    def apply[A](ff: FreeFunctor[F, A]): G[A] = ff match {
      case Map(f, g) => G.map(fg.apply(f))(g) 
    }
  }
  //With this transformation in hand we can now use the generic interpreter for our program:
  val consoleToIO: FreeConsole ~> IO = new (FreeConsole ~> IO) {
    def apply[A](c: FreeConsole[A]): IO[A] = c match {
      case FreeReadLine => IO(Option(readLine))
      case FreePrintLine(s) => IO(println(s))
    }
  }
  def compileConsole[A](p: FreeC[FreeConsole, A]): IO[A] = runFree[({type f[x]=FreeFunctor[FreeConsole,x]})#f,IO,A](p)(freeLift(consoleToIO)(IO))(IO)
  val compiledProgram: IO[Unit] = compileConsole(program)
  //going down to a fairly generic IO[Unit], all with an easygoing and nonredundant syntax
  
  //Another program example, file reader:
  trait Files[A]
  case class FreeReadLines(file: String) extends Files[List[String]]
  case class FreeWriteLines(file: String, lines: List[String]) extends Files[Unit]
  
  def fahrenheitToCelsius(x: Double): Double = ???
  
  val proggie = for {
    lines <- request(FreeReadLines("fahrenheit.txt"))
    cs = lines.map(s => fahrenheitToCelsius(s.toDouble).toString)
    _ <- request(FreeWriteLines("celsius.txt", cs))
  } yield ()
  
  //problem with this code: what if the file is huge? Infact what we are doing here is loading the whole file in mem.
  //Idea: expose lower controls:
  trait HandleR
  trait HandleW
  case class OpenRead(file: String) extends Files[HandleR]
  case class OpenWrite(file: String) extends Files[HandleW]

  case class ReadSingleLine(h: HandleR) extends Files[Option[String]]
  case class WriteSingleLine(h: HandleW, line: String) extends Files[Unit]
  
  //and we can for sure write a program for this combination too, the problem is that it will not compose, because
  //the following will be kind of monolithic loops
  
  def loop(f: HandleR, c: HandleW): FreeC[Files, Boolean] = ???
  def convertFiles: Unit = ???
  
  //So the takeaway is: it is of course cool to program with high level abstractions, but the tradeoff to a realistic
  //performance must be contemplated too.

  //What we showed with Free & c gives examples of programs which actually break referential transparency when used
  //with a certain kind of side-effecting functions. If you take the program on its own, then it does not break RT,
  //in some sense the program is "unaware" that it may change the external state. So, a program can be seen RT-valid
  //with respect to some class of programs it makes use of, and when it isn't, its exact part to be non-RT shall be
  //clearly spotted.
  
  //Let's introduce a new datatype for clean scoping of mutable effects. Say we are in some portion of the code which
  //violates RT. With this type we want to enforce the two following points:
  //-if a reference to an external mutable object is held then modification to it cannot be seen from any other point
  //-a mutable object can never be reachable outside of its creation scope
  //An example for the first case: an in-place sorting algorithm. The second: a low-level reader whose array return
  //is a structure which is modified "under" it but still the changes are not supposed to be observable by the upper
  //levels; the structure is "recycled" just for efficiency.
  
  //The starting point can be a modified state monad, we call it "state transition":
  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S) //protected because we don't want expose the possibility to change the state!
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      } 
    }
    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    } 
  }
  
  //Example application for ST: mutable memory cells handler. Operations:
  //-allocate
  //-read
  //-write
  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        cell = a
        ((), s)
      } 
    }
  }
  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {var cell = a })
  }

  //The read and write method themselves are pure! They just return ST's. S is to be seen as a token which allows to
  //modify the state. The initial state is given with apply. An arbitrary simple program:
  for {
    r1 <- STRef[Nothing, Int](1)
    r2 <- STRef[Nothing, Int](1)
    x  <- r1.read
    y  <- r2.read
    _  <- r1.write(y+1)
    _  <- r2.write(x+1)
    a  <- r1.read
    b  <- r2.read
  } yield (a, b)
  //As we have chosen a token of type Nothing, this program is impossible to run. But anyway, run is protected!!
  
  //First of all we cannot have A a mutable type, because this means exposing the state we modify internally. In
  //general we want to avoid ST[S, T] with T function of S. Let's have a trait for actions safe to run:
  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }
  //The sequence above therefore becomes:
  val stProgram = new RunnableST[(Int, Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x  <- r1.read
      y  <- r2.read
      _  <- r1.write(y+1)
      _  <- r2.write(x+1)
      a  <- r1.read
      b  <- r2.read
    } yield (a,b)
  }
  
  //Let's create a method to run an ST by calling its apply method: it is the only way to access the mutable computed
  //state
  object ST {
    def apply[S, A](a: => A) = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S) = (memo, s)
      }
    }
    def runST[A](st: RunnableST[A]): A = st.apply[Null].run(null)._1
  }

  //Running the program above:
  val (intA, intB) = ST.runST(stProgram)
  
  //A program which wants to return a mutable reference simply cannot be run:
  //new RunnableST[STRef[Nothing, Int]] { def apply[S] = STRef(1) } doesn't compile
  //STRef can never "escape" as it is tagged with the type S of the ST action it lives in; an STRef not giving errors
  //is guaranteed to be inside of the ST action which defines it
  //This compile check can be bypassed with the wildcard type, but the execution will fail  

  //A more useful application: mutable arrays.
  sealed abstract class STArray[S, A : Manifest] {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s) 
      }
    }
    def read(i: Int): ST[S, A] = ST(value(i))
    def freeze: ST[S, List[A]] = ST(value.toList)
    //EX 114 Apply a Map onto the Array: pair (k, v) puts value v in position k
    def fill(smap: SMap[Int, A]): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S) = {
        smap.foreach { case (k, v) => value(k) = v }
        ((), s)
      }
    }
    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }
  object STArray {
    def apply[S, A : Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] {
        lazy val value = Array.fill(sz)(v)
      }
    )
    def fromList[S, A : Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
    //EX 115: let's start to put together our supadupa quicksort. I've copied it from the solutions honestly. Too
    //much headache with quicksort per se.
    private def noop[S] = ST[S, Unit](())
  
    private def partition[S](a: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
      vp <- a.read(pivot)
      _ <- a.swap(pivot, r)
      j <- STRef(l)
      _ <- (l until r).foldLeft(noop[S])((s, i) => for {
        _ <- s
        vi <- a.read(i)
        _  <- if (vi < vp) (for {
          vj <- j.read
          _  <- a.swap(i, vj)
          _  <- j.write(vj + 1)
        } yield ()) else noop[S]
      } yield ())
      x <- j.read
      _ <- a.swap(x, r)
    } yield x
  
    private def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
      pi <- partition(a, l, r, l + (r - l) / 2)
      _ <- qs(a, l, pi - 1)
      _ <- qs(a, pi + 1, r)
    } yield () else noop[S]
  
    def quicksort(xs: List[Int]): List[Int] =
      if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
        def apply[S] = for {
          arr    <- STArray.fromList(xs)
          size   <- arr.size
          _      <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
    })
  }
  
  //So far we have taken into account println's and stuff as side effects. At a higher and extra-language level,
  //also writing to the memory accounts for this, and you can see this by using the eq method, which implements
  //referential transparency from Java. So reference assignment, which takes place at object creation, is a side
  //effect, not observable if the rest of the progam explicitly calls the method. And in general referential
  //transparency is always with respect to some context> we could have a context where we ignore IO as a global state
  //changer.
  
  //We now navigate on in an attempt to regain the compositionality lost with the IO-style of writing programs above.
  //In this sense, the problem is that such programs are basically just the embedding  of the imperative paradigm onto
  //the functional, a new way to do the old thing.
  //The data structure we use will be:
  type Task[A] = Free[Parallel, Either[Throwable, A]]
  object Task { 
    def apply[A](a: => A): Task[A] = ???
    def run[A](t: Task[A]): A = ???
  }
  //Now if we remember that Free[Parallel = IO[ it's easy to see that this structure here models an input/output
  //operation safe both with respect to memory consumption (Free) and exception handling (Either).
  //First example: is a file longer than n lines? Task may contain any impure piece of Scala code in the sense that
  //we are Free (aha) to use some imperative style too (hah), so something like:
  def linesGt40k(filename: String): Task[Boolean] = Task {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next
        count += 1 
      }
      count > 40000
    }
    finally src.close
  }
  //This program of course tells you if the file has more than 40k lines. The bad thing can be argued is the fact that
  //it is quite low level and imperative. But there are some good things too:
  //-linear
  //-lazy (we use an iterator, therefore we don't load the whole file in memory)
  //-early-terminating
  
  //Important point: in this program we have an explicit resource management, we load the file and we must close it.
  //It would be great if we could ensure this at compile time: when we are done with a program it automatically closes
  //the resources it is working with. Another thing we don't really like is that, as usual with imperative styles,
  //the domain-business and infrastructure are intertwined, the same program takes care of both executing some algo
  //and housekeep.
  
  //Say instead the file is represented by a stream already. We can have a much cooler program:
  def linesGt40k(file: SStream[String]): Boolean = file.zipWithIndex.exists(_._2 + 1 >= 40000)
  //and we can easily make variations:
  def linesGt40kNonempty(file: SStream[String]): Boolean = file.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)
  //But this is a free hypothesis, that we have a Stream already. We could cheat with something like:
  def lines(filename: String): Task[SStream[String]] = Task {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; SStream.empty[String] }
  }
  //This way we gain a little composability, but the file is closed only if we read through all of it. Another bad
  //point is that the stream could be traversed multiple times, and possibly in multiple threads. This style is called
  //lazy IO.

  //We introduce now the concept of stream transducer, simply a non-unfolding stream transformation. The core design:
  trait Process[I, O] {
    //Process driver
    def apply(s: SStream[I]): SStream[O] = this match {
      case Halt() => SStream()
      case Await(recv, fallback) => s match {
        case h #:: t => recv(h)(t)
        case _ => fallback(s) // Stream is empty
      }
      case Emit(h, t) => h.toStream append t(s)
    }
    def map[O2](f: O => O2): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h map f, t map f)
      case Await(recv, fb) => Await(recv andThen (_ map f), fb map f)
    }
    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => emitAll(h, t ++ p)
      case Await(recv, fb) => Await(recv andThen (_ ++ p), fb ++ p)
    }
    private def emitAll[I, O](head: Seq[O], tail: Process[I, O] = Halt[I, O]()): Process[I, O] = tail match {
      case Emit(h2, tl) => Emit(head ++ h2, tl)
      case _ => Emit(head, tail)
    }
    private def emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()): Process[I, O] = emitAll(SStream(head), tail)
    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) =>
        if (h.isEmpty) t flatMap f
        else f(h.head) ++ emitAll(h.tail, t).flatMap(f)
      case Await(recv, fb) =>
        Await(recv andThen (_ flatMap f), fb flatMap f)
    }
    def unit[O](o: => O): Process[I, O] = emit(o)
    //EX 116: fuse together the actions of two Process'es: we feed the output of the first into the second; this is
    //the analogous of function compositions - actually both Process and Function span two distinct categories.
    def feed(in: Seq[I]): Process[I, O] = Emit(apply(in.toStream).toSeq)
    //My solution is:
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt() => Halt()
      case Emit(h, t) => Emit(h, this |> t)
      case Await(recv, fb) => this match {
        case Emit(h, t) => t |> h.map(recv).foldLeft(Process.empty[O, O2])(_++_)
        case Halt() => Halt() |> fb
        case Await(recvNu, _) => Await(i => recvNu(i) |> p2) 
      }
    }
    //which is analogous to the github one the main difference being that I stick to the domain version below, which
    //must have been changed in some subsequent revision, you can check it out here
    //https://github.com/fpinscala/fpinscala/blob/f71225c51978b283bf11f633a31cd716b8656f74/exercises/src/main/scala/fpinscala/streamingio/StreamingIO.scala#L213
    
    //Any Function[I, O] which we would use in the classic pattern-match style can be defined as a Process[I, O]:
    def lift[I, O](f: I => O): Process[I, O] = Await((i: I) => emit(f(i), lift(f)))
    //An infinite Process execution could be defined as:
    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv, fb) => Await(recv andThen go, fb)
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }
    //With this at hand we can easily define a classic lazy operation like filter:
    def filter[I](f: I => Boolean): Process[I, I] = Await[I, I](i => if (f(i)) emit(i) else Halt()) repeat
    //and an example usage is: filter(_ % 2 == 0) |> lift(_ + 1)
    //EX 117 - some old friends
    def take(n: Int): Process[I, O] = 
      if (n < 0) this
      else this match {
        case h@Halt() => h
        case Emit(h, t) if h.size >= n => Emit(h take n, Halt())
        case Emit(h, t) => Emit(h, t take(n - h.size))
        case Await(recv, fb) => Await(recv andThen {p => p.take(n)}, fb.take(n))
      }
    def drop(n: Int): Process[I, O] =
      if (n < 0) this
      else this match {
        case h@Halt() => h
        case Emit(h, t) if h.size >= n => Emit(h drop n, t)
        case Emit(h, t) => Emit(Seq(), t drop (n - h.size))
        case Await(recv, fb) => Await(recv andThen {p => p.drop(n)}, fb.drop(n))
      }
    //EX 118 - cunt 
    //returns the single count:
    def count: Process[I, Int] = {
      def go(acc: Int): Process[I, Int] = Await(_ => emit(acc+1, go(acc+1)))
      go(0)
    }
    //The go-recursive functions above and below can of course be generalized; the engine can be something like:
    def loop[S, T](z: S)(f: (I, S) => (T, S)): Process[I, T] = Await((i: I) => f(i, z) match { case (o, s2) => emit(o, loop(s2)(f)) })
    //and count simply becomes:
    def loopCount: Process[I, Int] = loop(0)((_, s) => (s+1, s+1))
    //EX BONUS - esiste ?! As a rule of thumb, think of the go-way when intersted in I, the match on the types when
    //interested in O; when interested in the structure (eg length) both work but the go turns out easier. In this
    //particular case we are interested in a bounded loop, ie we should stop the traversal as soon as we found a hit,
    //which means that the fallback of the emit should be the halting state. This can of course be generalized in a
    //condLoop, but for simplicity:
    def exists[S](z: S)(p: I => Boolean): Process[I, Boolean] = Await{(i: I) =>
      val v = p(i)
      val next = if (v) Halt[I, Boolean]() else exists(v)(p)
      emit(v, next)
    }
    //If the element exists, then the last element of the list will be true, if not they will all be false (it may
    //also make sense to return only a single value, but that another function skeleton).
  }
  
  object Process {
    //This monad instance closely resembles the one for List, with the difference that it accepts a input and alsos
    //it can arbitrarily apply that concerning only with types, not implicit relations.
    def monad[I]: Monad[({ type f[x] = Process[I, x]})#f] = new Monad[({ type f[x] = Process[I, x]})#f] {
      override def unit[O](o: => O): Process[I, O] = Emit(Seq(o))
      override def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
    }
    def empty[I, O]: Process[I, O] = Halt[I, O]()
    //the sum "so far"
    def sum(p: Process[Double, Double]): Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] = Await((d: Double) => p.emit(d+acc, go(d+acc)))
      go(0.0)
    }
    //EX 119 - mav, the average "so far"
    def mean(p: Process[Double, Double]): Process[Double, Double] = {
      def go(count: Int, mean: Double): Process[Double, Double] = Await{d: Double => 
        val newMean = (mean*count+d)/(count+1)
        p.emit(newMean, go(count+1, newMean))
      }
      go(0, 0.0)
    }
    //Ok this is bad I know, these methods should live here
    def lift[I, O](f: I => O): Process[I, O] = ???
    def filter[I](f: I => Boolean): Process[I, I] = id.filter(f)
    def count[I]: Process[I, Int] = id.count
    def exists[I](p: I => Boolean): Process[I, Boolean] = id.exists(false)(p)
    def id[I]: Process[I, I] = lift(identity)
  }
  
  case class Emit[I, O](
    head: Seq[O],
    tail: Process[I, O] = Halt[I, O]()
  ) extends Process[I, O]
  case class Await[I, O](
    recv: I => Process[I, O],
    fallback: Process[I, O] = Halt[I, O]()
  ) extends Process[I, O]
  case class Halt[I, O]() extends Process[I, O]
  //I is the type of the input stream, O the output. This represents a state machine which needs to be interpreted by
  //some driver. You can see above an example driver, toghether with various functional-commodity methods, which 
  //define Process as a monad; we can also get the instance as for other monads above.
  
  //We introduced the concept of "transducer" as a stream transformer. Usually the stream we operate on is the result
  //of some operation, again for example reading the file lines. The original source therefore drives the computation
  //to say so. Let's introduce new abstractions for this:
  trait Source[O] {
    def |>[O2](p: Process[O, O2]): Source[O2]
    def count = this |> Process.count
    def filter(f: O => Boolean) = this |> Process.filter(f)
    def exists(p: O => Boolean) = this |> Process.exists(p)
    def map[O2](f: O => O2) = this |> Process.lift(f)
    def observe[O](snk: Sink[O]): Source[O]
    def run: Task[Unit]
  }
  object Source {
    //Reading file lines
    def lines(filename: String): Source[String] = ResourceR(
      Task(io.Source.fromFile(filename)),
      (src: io.Source) => Task(src.close),
      (src: io.Source) => {
        lazy val iter = src.getLines
        Task[Option[String]] { 
          if (iter.hasNext) Some(iter.next)
          else None
        }
      },
      Process.id[String]
    )
  }
  case class ResourceR[R, I, O]( // A resource from which we can read values
    acquire: Task[R],
    release: R => Task[Unit],
    step: R => Task[Option[I]],
    trans: Process[I, O]
  ) extends Source[O] {

      def observe[O](snk: all.about.scala.FPiS.Sink[O]): all.about.scala.FPiS.Source[O] = ???
      def run: all.about.scala.FPiS.Task[Unit] = ???
    
      def |>[O2](p: Process[O, O2]) = ResourceR(acquire, release, step, trans |> p)
    
      def collect: Task[IndexedSeq[O]] = {
        def tryOr[A](a: => A)(cleanup: Task[Unit]) = try a catch { case e: Exception => Task.run(cleanup); throw e }
        @annotation.tailrec
        def go(
          acc: IndexedSeq[O],
          step: Task[Option[I]],
          p: Process[I, O],
          release: Task[Unit]
        ): IndexedSeq[O] = p match {
          case Halt() => Task.run(release); acc
          case Emit(h, t) => go(acc ++ h, step, t, release)
          case Await(recv, fb) => tryOr(Task.run(step))(release) match {
            case None => go(acc, Task(None), fb, release)
            case Some(i) => go(acc, step, tryOr(recv(i))(release), release)
          }
        }
        //Notice how we are guaranteed to release the resource
        acquire map { 
          case l@Left(_) => l 
          case Right(res) => Right(go(IndexedSeq(), step(res), trans, release(res)))
        }
      }
    
  }
  
  //The count lines snippet above now becomes:
  Source.lines("input.txt").count.exists(_ > 40000)

  //A use case: transform a file of C temperatures in F, ie map a file into another. Our source will be:
  val tempsC: Source[Double] = Source.lines("fahrenheit.txt").filter(!_.startsWith("#")).map(s => fahrenheitToCelsius(s.toDouble))
  //Ok for the read, but what about outputing to the file? Let's introduce a Sink type as well!
  trait Sink[I] {
    def <|[I0](p: Process[I0,I]): Sink[I0]
    def filter(f: I => Boolean) = this <| Process.filter(f)
  }
  case class ResourceW[R, I, I2](
    acquire: Task[R],
    release: R => Task[Unit],
    recv: R => (I2 => Task[Unit]),
    trans: Process[I, I2]
  ) extends Sink[I] {
    def <|[I0](p: Process[I0, I]) = ResourceW(acquire, release, recv, p |> trans)
  }
  //And here's a combinator dual to the lines method above:
  object Sink {
    def file(filename: String, append: Boolean = false): Sink[String] = ResourceW(
      Task(new FileWriter(filename, append)),
      (w: FileWriter) => Task(w.close),
      (w: FileWriter) => (s: String) => Task(w.write(s)),
      Process.id[String]
    )
  }
  
  //How shall we combine them? Check out the "observe" method above. The program would then be:
  val convert: Task[Unit] =
    Source.lines("fahrenheit.txt").
    filter(!_.startsWith("#")).
    map(s => fahrenheitToCelsius(s.toDouble)).
    map(d => d.toString + "\n").
    observe(Sink.file("celsius.txt")).
    run

  //So this file mapping scenario is covered, but there is quite some related use cases which we cannot satisfy, for
  //example:
  //-generalizing to multiple inputs, eg zip more files together; same as for concatenation
  //-multiple sinks
  //-recursion: composing the sinks with the sources
  //Therefore, let's backtrack a little and consider a new way of doing business with Process.
    
  //The Process type above works with an input stream or, more generally, some context. It processes the information
  //source according to a fixed protocol defined by the three instances above. To gain a little more freedom though,
  //we can parametrize on the way the input is handled, thinking of an arbitrary container on it. As a first step:
  trait ProcessF[F[_], O] {
    def emitAll[F[_], O](head: Seq[O], tail: ProcessF[F,O] = HaltF[F, O]()) = tail match {
      case EmitF(h2, t) => EmitF(head ++ h2, t)
      case _ => EmitF(head, tail)
    }
    def emit[F[_], O](head: O, tail: ProcessF[F, O] = HaltF[F, O]()) = emitAll(List(head), tail)
    //A new utility:
    def await[F[_], A, O](req: F[A])(recv: A => ProcessF[F, O] = (a: A) => HaltF[F, O](), fallback: ProcessF[F, O] = HaltF[F, O](), cleanup: ProcessF[F, O] = HaltF[F, O]()): ProcessF[F, O] = AwaitF(req, recv, fallback, cleanup)
    def Try[F[_],O](p: => ProcessF[F, O]): ProcessF[F, O] = try p catch { case e: Throwable => HaltF(e) }
    def ++(p: => ProcessF[F, O]): ProcessF[F, O] = this match {
      case HaltF(End) => Try(p)
      case HaltF(err) => HaltF(err)
      case EmitF(h, t) => emitAll(h, t ++ p)
      case AwaitF(req, recv, fb, c) => AwaitF(req, recv andThen (_ ++ p), fb ++ p, c)
    }
    //map - see above
    def map[O2](f: O => O2): ProcessF[F, O2] = ???
    //The flatMap definition is pretty much what we have above, just this is safe - notice the Try's
    def flatMap[O2](f: O => ProcessF[F, O2]): ProcessF[F, O2] = this match {
      case HaltF(err) => HaltF(err)
      case EmitF(Seq(o), t) => Try(f(o)) ++ t.flatMap(f)
      case EmitF(o, t) =>
        if (o.isEmpty) t.flatMap(f)
        else Try { f(o.head) } ++ emitAll(o.tail, t).flatMap(f)
      case AwaitF(req, recv, fb, c) =>
        AwaitF(req, recv andThen (_ flatMap f), fb flatMap f, c flatMap f)
    }
    def filter[I, F[_]](f: I => Boolean): ProcessF[F, I] = ???
    //The appended ProcessF is executed both in success and failure
    def onComplete(p: => ProcessF[F, O]): ProcessF[F, O] = this match {
      case HaltF(End) => Try(p)
      case HaltF(err) => Try(p) ++ HaltF(err)
      case EmitF(h, t) => emitAll(h, t onComplete p)
      case AwaitF(req, recv, fb, c) => AwaitF(req, recv andThen (_ onComplete p), fb onComplete p, c onComplete p)
    }
    def resource[R, O](acquire: Task[R])(release: R => ProcessF[Task, O])(use: R => ProcessF[Task, O]): ProcessF[Task, O] = 
      await[Task, R, O](acquire)(r => use(r).onComplete(release(r)))
    def repeat: ProcessF[F, O] = {
      def go(p: ProcessF[F, O]): ProcessF[F, O] = p match {
        case HaltF(_) => go(this)
        case AwaitF(rq, recv, fb, c) => AwaitF(rq, recv andThen go, fb, c)
        case EmitF(h, t) => EmitF(h, go(t))
      }
      go(this)
    }
    def |>[O2](p2: ProcessF[F, O2]): ProcessF[F, O2] = ???
    //Ok ok this should be in the companion etc etc etc
    def lines(filename: String): ProcessF[Task, String] = resource
      { Task(io.Source.fromFile(filename)) }
      { src => await[Task, Unit, String](Task(src.close))() } //the cleanup just evaluates a Task action, n then stop
      { src =>
        lazy val iter = src.getLines
        def step = if (iter.hasNext) iter.next else throw End
        //evaluate repeatedly to produce the next line; this is the promotion of a Task to a Process; see exercise 120
        //for a general implementation
        await[Task, String, String](Task(step))(emit(_)).repeat
      }
    //EX 120 Same for this one, again if you look at the book alone a lot of methods are supposed to float in between
    //the lines
    def eval[F[_], A](a: F[A]): ProcessF[F, A] = await[F, A, A](a) {
      case Left(err: Throwable) => HaltF(err)
      case Right(a: Seq[A]) => EmitF(a, HaltF(End))
    }
    //This is supposed to just not emit anything
    def eval_[F[_], A, B](a: F[A]): ProcessF[F, B] = eval[F, A](a).drain[B]
    
    //A Sink writing to a file - move it to some companion
    def fileW(file: String, append: Boolean = false) = resource
      { Task(new FileWriter(file, append)) }
      { w => eval[Task, Unit](Task(w.close)).drain[String => ProcessF[Task, Unit]] }
      { w => constant { (s: String) => eval[Task, Unit](Task(w.write(s))) }}
    def constant[A](a: A): ProcessF[Task, A] = eval[Task, A](Task(a)).repeat //an infinite constant stream
    //Piping the output of a ProcessF to a SinkF
    def zipWith[O2, O3](p2: SinkF[F, O2])(f: (O, O2) => O3): SinkF[F, O3] = ???
    def to[O2](sink: SinkF[F, O]) = ???
    //EX 121 a new operator - concatenates a nested process (see the joins above)
    def join[F[_], O](p: ProcessF[F, ProcessF[F, O]]): ProcessF[F, O] = p.flatMap(identity)
    
    //Abstractions for Process1
    @annotation.tailrec
    final def kill[O2]: ProcessF[F, O2] = this match {
      case AwaitF(req, recv, fb, c) => c.drain
      case HaltF(_) => HaltF()
      case EmitF(h, t) => t.kill
    }
    def drain[O2]: ProcessF[F, O2] = this match {
      case HaltF(_) => HaltF()
      case EmitF(h, t) => t.drain
      case AwaitF(req, recv, fb, c) => AwaitF(req, recv andThen (_.drain), fb.drain, c.drain)
    }
    def feed[O2](in: Seq[O]): ProcessF[F, O2] = ??? //not really sure
    private def Get[I] = Is[I]().Get
    def await1[I](recv: I => Process1[I, O], fb: Process1[I, O] = HaltF[Is[I]#f, O]()): Process1[I, O] = await(Get[I])(recv, fb, halt1)
    def emit1[I](h: O, tl: Process1[I, O] = halt1): Process1[I, O] = emit(h, tl)
    def emitAll1[I](h: Seq[O], tl: Process1[I, O] = halt1): Process1[I, O] = emitAll(h, tl)
    def halt1[I]: Process1[I, O] = HaltF[Is[I]#f, O]()
    def lift[I](f: I => O): Process1[I, O] = await1((i: I) => emit[Is[I]#f, O](f(i))) repeat
    def |>>[O2](p2: Process1[O, O2]): ProcessF[F, O2] = p2 match {
      case HaltF(e) => this.kill ++ HaltF(e)
      case EmitF(h, t) => emitAll(h, this |>> t)
      case AwaitF(req, recv, fb, c) => this match {
          case EmitF(h, t) => ??? //t |>> feed(h)(p2) this apply is missing for me - see fpinscala on github
          case HaltF(End) => HaltF(End) |>> fb
          case HaltF(err) => HaltF(err) |>> c
          case AwaitF(req0, recv0, fb0, c0) => await(req0)(i => recv0(i) |>> p2, fb0 |>> fb, c0 |>> c)
      }
    }
    
    //Abstractions for Tee
    def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = await[T[I, I2]#f, I, O](L)(recv, fallback)
    def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = await[T[I, I2]#f, I2, O](R)(recv, fallback)
    def haltT[I, I2, O]: Tee[I, I2, O] = HaltF[T[I, I2]#f, O]()
    def emitT[I, I2, O](h: O, tl: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = emit(h, tl)
    //This one here is peculiar of Tee; the order left-right is explicit
    def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] = awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i,i2)))) repeat
    def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_,_))
    def zipWithAll[I, I2, O](padI: I, padI2: I2)(f: (I, I2) => O): Tee[I, I2, O] = {
      val fbR = passR[I, I2] map (f(padI, _    ))
      val fbL = passL[I, I2] map (f(_   , padI2))
      awaitLOr(fbR)(i => awaitROr(fbL)(i2 => emitT(f(i,i2)))) repeat
    }
    //Ignore one of the two branches
    def passR[I, I2]: Tee[I, I2, I2] = awaitR(emitT(_, passR))
    def passL[I, I2]: Tee[I, I2, I] = awaitL(emitT(_, passL))
    //Sugarcoated await
    def awaitLOr[I, I2, O](fallback: Tee[I, I2, O])(recvL: I => Tee[I, I2, O]): Tee[I, I2, O] = awaitL(recvL, fallback)
    def awaitROr[I, I2, O](fallback: Tee[I, I2, O])(recvR: I2 => Tee[I, I2, O]): Tee[I, I2, O] = awaitR(recvR, fallback)
    //Process combining - notice how we make sure to run cleanup on both processes before halting:
    def tee[O2, O3](p2: ProcessF[F, O2])(t: Tee[O, O2, O3]): ProcessF[F, O3] = t match {
      case HaltF(e) => this.kill ++ p2.kill ++ HaltF(e) //if t halts, gracefully kill off both inputs
      case EmitF(h, t) => EmitF(h, (this tee p2)(t)) //emit any leading values, the recurse
      case AwaitF(side, recv, fb, c) => side.get match { //we check whether the request is for the left or right side
        case Left(isO) => this match { //it's a request from the left Process, and we get a witness that recv takes O
          case HaltF(e) => p2.kill ++ HaltF(e) //the Tee is requesting input from the left, which is halted, so halt
          //there are values available, so feed them to the Tee
          case EmitF(o, ot) => ??? //(ot tee p2)(feedL(o)(t)) this apply is missing for me - see fpinscala on github
          case AwaitF(reqL, recvL, fbL, cL) => 
            //no values are currently available, so wait for a value, then continue with the tee operation
            AwaitF(reqL, recvL andThen (this2 => (this2 tee p2)(t)), (fbL tee p2)(t), (cL tee p2)(t))
        }
        //it's a request from the right Process, and we get a witness that recv takes an O2; otherwise go as above
        case Right(isO2) => p2 match {
          case HaltF(e) => this.kill ++ HaltF(e) 
          case EmitF(o, ot) => ??? //(this tee ot)(feedR(o)(t)) this apply is missing for me - see fpinscala on github
          case AwaitF(reqR, recvR, fbR, cR) => AwaitF(reqR, recvR andThen (p3 => (this tee p3)(t)), (this tee fbR)(t), (this tee cR)(t))
        }
      }
    }
    
  
  }
  object ProcessF {
    def collect[O](src: ProcessF[Task, O]): IndexedSeq[O] = {
      @annotation.tailrec
      def go(cur: ProcessF[Task, O], acc: IndexedSeq[O]): IndexedSeq[O] =
        cur match {
          case EmitF(h,t) => go(t, acc ++ h)
          case HaltF(End) => acc //normal termination, return the accumulated values
          case HaltF(err) => throw err //process completed with error, escalate
          case AwaitF(req, recv, fb, c) =>
            val next = try recv(Task.run(req)) catch { 
              case End => fb //input exhausted normally, call the "on done" hook
              case err: Throwable => c ++ HaltF(err) //with error, run the cleanup and return the error
            }
            go(next, acc)
        }
        go(src, IndexedSeq())
    }
    def collect[F[_], O](src: ProcessF[F, O])(implicit F: Monad[F], P: Partial[F]): F[IndexedSeq[O]] = {
      def go(cur: ProcessF[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
        case EmitF(h, t) => go(t, acc ++ h)
        case HaltF(End) => F.unit(acc) //normal termination, return acc
        case HaltF(err) => P.fail(err) //if error raise it
        case AwaitF(req, recv, fb, c) => F.flatMap (P.attempt(req)) { //we don't run the task, rather we flatMap on it
          case Left(End) => go(fb, acc) //if completed call fallback
          case Left(err) => go(c ++ src.await[F, Nothing, O](P.fail(err))(), acc) //if error cleanup and rethrow 
          case Right(o) => go(TryAwait(recv(o))(fb, c), acc) //catch and handle recv exceptions
        }
      }
      go(src, IndexedSeq())
    }
    def id[F[_], I]: Process[F[I], I] = ???
  }
  case class AwaitF[F[_], A, O](
    req: F[A], 
    recv: A => ProcessF[F, O],
    fallback: ProcessF[F, O],
    cleanup: ProcessF[F, O] //the cleanup is executed if the request fails with an error
  ) extends ProcessF[F, O]
  case class EmitF[F[_], O](
    head: Seq[O],
    tail: ProcessF[F, O]
  ) extends ProcessF[F, O]
  case class HaltF[F[_], O](err: Throwable = End) extends ProcessF[F, O]
  case object End extends Exception //normal termination
  //The type F comes to the define the protocol we use to interface with the source I in the process of transforming
  //it into O. There is an analogy between what F is for await and the F from Free for Suspend above. The two
  //abstractions are of course conceptually different though. For example, with Process we are able to emit multiple
  //values, in possibly different chunks as we are using Seq, whereas Free, deep inside the multiple layers contains
  //always a single and fixed answer, returned with Return; Return is also the final state for a Free machine, whereas
  //Halt is it for the Process machine: this means that no matter what when the computation is over we always get back
  //some value with Free, instead with Process what we have is a termination in the strict sense.
  //We can define all of the basic combinators defined for Process[Id], see above.

  //If before, with the simple Process, we had to introduce a dedicated Source type now we can represent it directly
  //with the F container, using Task for example. The source is the "external" world, and what we do is to make
  //requests to it by executing Task's. So now ProcessF[Task, O] *is* the actual source! See the companion for an
  //interpreter.
  //We can as well elaborate a little more on what F should be, for now it is really generic. We introduce:
  trait Partial[F[_]] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
  }
  //see above for an update version of collect.
  //TryAwait - used in the Partial-version of the interpreter
  def TryAwait[F[_], O](p: => ProcessF[F, O])(fallback: ProcessF[F, O], cleanup: ProcessF[F, O]): ProcessF[F, O] = try p catch {
    case End => fallback
    case e: Throwable => cleanup ++ HaltF(e)
  }
  //In the ProcessF we already have the machinery to ensure resource safety (eg no leaks), namely the cleanup argument
  //in AwaitF. First of all, a resource shall be closed when it has been entirely consumed, ie after emitting its
  //last value. This means that the fallback action of any AwaitF should execute the closing too. This, toghether with
  //the closing in the cleanup, assures us that we close the resource, come rain come shine. In general we can
  //introduce a new composition, similar to ++, by which the argument process is appended and executed in any case
  //(onComplete, see above).
  //We are now ready for (yet) another way of writing our fileread program, with the help of a new combinator, called
  //resource (above).
  //Let's now consider a line like:
  //collect(lines("names.txt") |> take(5))
  //it is here on the composed process take to call the cleanup. Now of course this cannot be enforced in the collect
  //method as the fact that a process is composed by multiple processes possibly requiring finalization is not exposed.
  //Thus a new rule must be explicitly satisfied: any process pulling data from another process must take care of the
  //cleanup action of the process it composes to before it halts itself. Of course it would be really bad if a given
  //process should take care of that. Since we always compose with |> that's where we want to frame this concept.
  
  //We introduced ProcessF as a means of wrapping the type I with a protocol, thus modeling the interaction with the
  //datasource. And what is the easiest protocol we can think of? What we already have with Process, a raw one. As
  //it is a protocol we do need to expose some operations, and while this can be arguably done with Id let's play
  //around and redesign it:
  case class Is[I]() {
    sealed trait f[X] { def is: Eq[X, I] }
    val Get: f[I] = new f[I] { def is = Eq.refl }
  }
  case class Eq[A, B](to: A => B, from: B => A) //DIY evidence that A and B can be considered equal
  object Eq { def refl[A]: Eq[A, A] = Eq(identity, identity) } //using identities forces a single type
  //We can simply define an alias now:
  type Process1[I, O] = ProcessF[Is[I]#f, O]
  
  //You can try to substitute Is[I]#f into AwaitF and see that it can only be used to pull pure I values.
  //All of the methods above are of course readily available - see ProcessF for various examples.
  
  //Another scenario we mentioned earlier is the possibility to compose two streams. The first abstraction could be:
  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[Eq[X, I], Eq[X, I2]] }
    val L = new f[I] { def get = Left(Eq.refl) }
    val R = new f[I2] { def get = Right(Eq.refl) }
  }
  def L[I, I2] = T[I, I2]().L
  def R[I, I2] = T[I, I2]().R
  type Tee[I, I2, O] = ProcessF[T[I, I2]#f, O]
  //So we are moving in the same way as above
  
  //As we suggested above, we can model data sinks still as Sources:
  type SinkF[F[_], O] = ProcessF[F, O => F[Unit]]
  //See ProcessF for some snippets 
  //Its generalization is:
  type Channel[F[_], I, O] = ProcessF[F, I => ProcessF[F,O]]
  //Dynamic resource allocation can also be part of the game with the abstractions we introduced (see book).
  
}