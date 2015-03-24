package all.about.scala

import math.Ordering.IntOrdering
import scalaz._
import Scalaz._

object ScalaZee {

  //Different types of polymorphism
  
	type Foo
	type Bar
	
  //Overloading:
  object Comparable {
    def compare(self: Foo, that: Foo): Foo = ???
    def compare(self: Bar, that: Bar): Bar = ???
  }
  
  //Subtyping:
  trait Comparable[A] {
    def compareTo(that: A): Int
  }
  trait FooComparable extends Comparable[Foo] {
    def compareTo(that: Foo): Int = ???
  }
  
  //And... typeclasses!
  trait Ordering[A] {
    def compare(self: A, that: A): Int
  }
  object FooOrdering {
    implicit val ord: Ordering[Foo] = ??? //this is called typeclass instance
  }
  //usage:
  def sort[A](xs: Seq[A])(implicit ord: Ordering[A]) = {
    val x = xs.head //xs nonempty of course...
    val y = xs.last //idem
    ord.compare(x, y)
  }
  
  //Coding with typeclasses. We all know what a Monoid is; scalaz has of course readymade instances for the main types
  val intMonoid: Monoid[Int] = intInstance
  //If you have some structure you can fold on, thus a Foldable, you can easily construct totals: for example, the
  //sum of a list
  
  //Let's create a Monoid instance ourselves!
  case class Max(value: Int) extends AnyVal
  object Max extends IntOrdering {
    implicit val monoid: Monoid[Max] = new Monoid[Max] {
      def zero = Max(0)
      def append(m1: Max, m2: Max) = Max(max(m1.value, m2.value))
    }
  }
  
  //A Functor is a higher-kinded type as it accepts a type parameter of arity nonzero in its definition.
  
  //Some of the standard Scala types have a functional nature which can be unveiled by importing 
  //scalaz.std.AllInstances._. List is a bridge type in the sense that a lot of this functional stuff is defined only
  //for List, not Seq or Vector for example.
  
  //Another pair of important abstractions are Applicative and Traverse
  val optionApplicative: Applicative[Option] = optionInstance
  optionApplicative.apply2(Some("alois"), Some(29)) {
    (name, age) => ???
  }
  Applicative[Option].apply2(Some("alois"), Some(29)) {
    (name, age) => ???
  }
  //Syntax to the rescue!
  (some("alois") |@| some(29)) { (name, age) => ??? }

  //Finally, Traverse is meant to be used when you wanna swap two containers (not any container, see the definition):
  List(some("1"), some("2"), some("3")).sequence 
  //here we got to use some and not Some: it simply returns an Option so that the implicits, defined on Option, can
  //be properly resolved

}