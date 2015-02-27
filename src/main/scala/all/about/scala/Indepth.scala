package all.about.scala

import annotation._
import util.Try
import java.io.File
import Array.canBuildFrom

object Implicits {
  
  //Implicits for saving parameters keystrokes:
  def findAnInt(implicit x : Int) = x
  findAnInt(1) //ok, we don't have an Int on scope so we provide one
  implicit val i = 2 //now we have one!
  findAnInt //and we can make good use of it
  
  //Implicits are looked up in:
  //A: lookup site: in the same syntactical scope of the call site or an enclosing one (like above)
  //B: implicit scope of the type: the number of companion objects associated with the type:
  // 0: companion object of the type 
  // 1: companion objects of extended types: T extends A with B with C, we look into the companions of A, B and C
  // 2: if the type is parametric, the same recursion on the companions of its parts is performed
  // 3: we also look for implicits where type is defined and its enclosings too
  
  //Ex B2 - implicit scope via type parameters
  object Holder {
    trait Foo
    object Foo {
      implicit val list: List[Foo] = List(new Foo{})
    }
  }
  implicitly[List[Holder.Foo]]
  //type List[T] parametric in T for T fixed to Holder.Foo here, so we look into the Holder.Foo companion. This style
  //is what we use when working with type classes in Scala (later).
  
  //Ex B3 - implicit scope via nesting
  object Houder {
    trait Foo
    implicit def foo: Foo = new Foo{}
  }
  implicitly[Houder.Foo]
  //this technique is a shortcut for defining the implicit inside Foo's companion (B0) as in:
  object Houda {
    trait Foo
    object Foo {
      implicit val foo: Foo = new Foo{}
    }
  }
  implicitly[Houda.Foo]
  
  //Defining implicit for objects (this is basically 3B again):
  object FOO {
    object BAR { override def toString = "IL TOSTRING" }
    implicit def bar: BAR.type = BAR
  }
  implicitly[FOO.BAR.type]
  
  //In Scala packages can be defined inline and have companion objects too, and these companions make good place where
  //to put implicit definitions to be in scope in all of the contained entities (the following does not compile because
  //we are inside an object here)
//  package sixpack {
//    class Foo {
//      override def toString = "TS"
//    }
//  }
//  package object sixpack {
//    implicit def foo = new Foo
//  }
  //There can be multiple package definitions, that are merged, but only one package object definition per package.
  //Dropping implicits in the package objects is the way to go for libraries, much better than scattering these around
  //the single companion objects.
  
  //When dealing with views/implicit conversions the same lookup strategy is applied, and we examine the starting type,
  //not the destination.
  
  //The "implicit" pattern for libraries:
  //1 Define the core abstractions:
  //package complexmath { case class ComplexNumber(a: Double, b: Double) }
  //2 Define the needed implicit conversions, and the best place is a package object, so that all of them are visible
  //  and discoverable
  //3 Make sure not to create views that might conflict, ie between Scala API types and with generic names; make sure
  //  that nothing in scala.Predef is shadowed, this is imported by default in all Scala classes
  //package object complexmath { implicit def realToComplex(r : Double) = new ComplexNumber(r, 0.0) }
}

object Types {
  
  trait ItsAllAboutTypes {
	  class IAmAType
	  trait IAmATypeToo
	  object WellMeToo
	  type SameAsHere
	  
	  def a(a: IAmAType)
	  def b(b: IAmATypeToo)
	  def c(c: WellMeToo.type)
	  def d(d: SameAsHere)
  }
  
  //We can refer to a type in a path dependent way, the standard, or a projection:
  class Outer {
    trait Inner
    def y = new Inner {}
	  def foo(x : this.Inner) = null
	  def bar(x : Outer#Inner) = null
	  def baz(x: this.type) = null
  }
  val a, b = new Outer
  val ay = a.y; val by = b.y
  a.foo(ay) //OK, the foo method of an instance works with its own (this.) Inner
  //a.foo(by) it can't work with others!
  a.bar(by) //we have the #-projection method for this
  a.baz(a) //tricky, 'cause it might seem like the following is the right one:
  //a.baz(new Outer)
  //a.baz(b) explicit path dependency disallows this too
  
  trait StillTypes {
    type A //abstract
    type B = Int //alias
    type C[A] <: List[A] //type definitions can be parametric/higher kinded; <: this is a no-more-general-than
    type D >: AnyVal //conformance bound with >; is a no-more-specific-than
    type E = {
      val x: Int
      def y: Double
      type e
    } //structural
  }
  //Remember: the 1 of types is:
  type Amy = Any
  //and the 0:
  type nuttin = Nothing
  
  //Params n shit:
  def fed[A](a: A) = a
  
  def dfe[T[_]](f: T[Int]) = f //this takes an arbitrary type parametrized in Int, so we can have:
  type Callback[A] = Function1[A, Unit]
  val x : Callback[Int] = y => println(y + 2)
  dfe(x)
  //or even betta:
  def bettadfe[A, T[_]](f: T[A]) = f
  bettadfe(x)(1)
  val z : Callback[String] = y => println(y + 2)
  bettadfe(z)("")

  //anonymous types:
  dfe[({type X[Y] = Function1[Y, Unit]})#X]( (x : Int) => println(x) )
  //type is created on this "anonymous path" and this is referred with #
  
}

object Coooovariance {
  
  //As we (already) know:
  //-covariance: type can be substituted with its parent
  //-contravariance: parent can be substituted with child parent
  //-invariance: none of the two
  
  //Positions:
  trait T[+A] {
    //def thisWillNotWork(a : A) = a does not compile 
  }

  //At a later compile phase this will not pass because functions are invariant in the argument args and covariant
  //in the return types. This won't work for the same reason, and mutable classes must be invariant too!
  //class Mutable[+A](var m: A) won't compile
  
  //Every wonder how something like:
  List(1, 2, 3)++List("i", "love", "my", "fudge")
  //is possible?
  trait Listah[+ItemType] {
    def ++[OtherItemType >: ItemType](other: Listah[OtherItemType]): Listah[OtherItemType] 
  }
  val lizt: Listah[Any] = new Listah[Int]{def ++[OtherItemType >: Int](other: Listah[OtherItemType]): Listah[OtherItemType] = ???}++(new Listah[String]{def ++[OtherItemType >: String](other: Listah[OtherItemType]): Listah[OtherItemType] = ???})
  //So here the type of the argument to ++, named OtherItemType, is inferred to be Any by the compiler, the join of 
  //the two types, type Any. We can't just use a single type parameter and beg the compiler to infer this, to Any for
  //example, we would get the above error again.
  
  //A getter:
  class GetGot[A](i: A) {
    val getI: () => A = () => i
    //Remember: () is the only instance of Unit 
  }
  val anyGetGot = new GetGot[Any](1)
  val intGetGot = new GetGot[Int](1)
  //Getters are covariant, since they are functions: as an example, reminding that functions are covariant in their 
  //returns, we can have the following:
  def returnGet: () => Any = intGetGot.getI
  //Now, Int <: Any, getters are covariant so () => Int <: () => Any and the return of a function sending out an Any
  //getter can be an Int getter as well
  
  //Reifying Getters:
  trait Getter[A] {
    def a: A
    def getA(): () => A = () => a
    def map[B]: ((A => B) => (() => A) => () => B)
    def flatMap[B]: (() => A) => (A => (() => () => B)) => (() => B)
  }
  
  //A safe get:
  trait SafeGetter[A] {
    def safeGetA(): () => Try[A]
    def safeGetWithOption(): () => Try[Option[A]]
    def safeGetFactory: () => (() => Try[A]) = () => safeGetA()
  }
 
}

object Taipcless {
  //View bounds:
  def foo1[A <% Int](x: A) = x
  //equivalent to:
  def foo2[A](x: A)(implicit $ev0: A => Int) = x
  //and to (context bound):
  type X[A] = Function[A, Int]
  def foo3[A : X](x: A) = x
  //and of coursa:
  def foo4[A](x: A)(implicit $ev0: X[A]) = x
  //These definitions are really equivalent as you can see from the errors:
  //foo1("f")
  //foo2("f")
  //foo3("f")
  //foo4("f")
  //Accepting an implicit rather than explicit parameter is convention when the conversion function is needed yes but
  //never accessed and when working with type classes (view bounds) or providing implicit values in companions (context
  //bounds)
  
  //With 2.8 a dedicated machinery to carry around type information was introduced, Manifest's. A Manifest is used as
  //an implicit parameter to save info that would be deleted by erasure otherwise. The main application is dealing
  //with arrays, that in Scala are just Array[T], nothing elaborate, but on the JVM different types yield actually
  //different classes.: for example, Array[Double] and Array[Int] map to different bytecode classes double[] and int[]
  //the information is carried in the Manifest.
  
  //Manifest: for a type T[A1, ..., An] a reflective instance of T and of all of the types A1,..., An. A reflective 
  //instance is basically a runtime instance coming from the Class object:
  val loadedList = Class.forName("java.util.ArrayList")
  val list = loadedList.newInstance().asInstanceOf[java.util.ArrayList[Int]]
  //Example of Manifest use:
  //def arr[T] = new Array[T](0) nee, won't compile
  def arr[T](implicit m: Manifest[T]) = new Array[T](0) //ja
  //As an Array factory
  def arrFactory[T](implicit m: Manifest[T]) = m.newArray(10)
  //This works because we pass in a class type with an empty constructor; pass through getConstructor and friends to
  //get it working in the general case
  class AnArglessClass
  def phoo[T](implicit m: Manifest[T]) = m.runtimeClass.newInstance()
  phoo[AnArglessClass]
  //we can get Manifest's for the arg type too we said:
  def manargs[A, B, C](triple: (A, B, C))(implicit m: Manifest[(A, B, C)]) = m.typeArguments
  
  //ClassManifest: deprecated by ClassTag, cannot be always expected to store type info for more than the root, 
  //the result of erasure (eg only List in List[Int])

  //OptManifest: either a Manifest subclass or a NoManifest:
  case class Foo[A](a: A)
  type F = Foo[_]
  val foos: List[F] = List(Foo(1), Foo("a"), Foo('a))
  //toArray needs a Manifest, so we can say the compiler has the needed info; a ClassManifest is expected, in particular
  val foosArray = foos.toArray
  classManifest[F] 
  //returns Foo[<?>], Foo of NoManifest, because the compiler is unable to come up with a proper manifest here; this 
  //is because of Foo[_], Foo[Any] returns a Manifest for Any; it makes sense that ClassManifest returns an optional
  //manifest since it is not necessarily responsible for its subtypes
 
  //Don't be greedy, define multiple parameters lists:
  def greedy[A](xs: List[A], f: A => Boolean) = null
  def nongridi[A](xs: List[A])(f: A => Boolean) = null
  //greedy(List("String"), _.isEmpty) A was not inferred yet!
  nongridi(List("String"))(_.isEmpty) //now ok
  
  //You can have the same issue with type parameters:
  def peek[A, C <: Traversable[A]](col : C) = (col.head, col)
  //peek(List(1, 2, 3))
  //but multiple parameter lists for types are not supported. We rely on implicits and reification instead:
  def peekpik[C, A](col: C)(implicit ev: C <:< Traversable[A]) = (col.head, col)
  //which is a shorthand for: 
  def peekpik2[C, A](col: C)(implicit ev: <:<[C, Traversable[A]]) = (col.head, col)
  peekpik(List(1, 2, 3))
  //A reification is an entity representing a concept in the language:
  //- Function[A, B] is the reification of the space of methods from A to B
  //- <:< implements the lessOrEqual relationship, A <: B; if an instance of the abstract class <:<[A, B] can be found
  //  as an implicit then it means that A is a subclass of B
  //- =:= for type equality
  
  //<:< is a function and therefore contravariant in the argument, covariant in the result; now if A <: B then 
  //<:<[B,B] <: <:<[A,B], and the implicit lookup for <:<[B, A] is satisfied with <:<[A, A] itself (now this is some
  //ill trick)
  
  //A specialized method is one which works for a subtype of a specified type. For example:
  def sum[B >: {type A = Int}](implicit num: Numeric[B]): B = ???
  //this style of method is common in the collections, for example I can compute the total only of a list of numbers;
  //as we know nothing prevents us from defining a plus.
  //Such methods can be defined with the type constraint reification above too
  
  //The Scala-idiomatic typeclassing:
  //a parametrized trait which serves as an accessor/utility interface to a given type:
  @annotation.implicitNotFound(msg ="NO SERIALIZAHS FO ${T} FOO")
  trait FileLike[T] {
    def name(file: T): String
    def isDirectory(file: T): Boolean
    def ls(file: T): Seq[T]
  }
  //a companion containing actual trait implementations for all of the desired types
  object FileLike {
    implicit val fileFileLike = new FileLike[File] {
	    def name(file: File): String = file.getName
	    def isDirectory(file: File): Boolean = file.isDirectory
	    def ls(file: File): Seq[File] = file.listFiles
    }
    //other implicits for other types...
  }
  //methods using the interface look up implementations as implicits
  
  def synchronize[F : FileLike, G: FileLike](from: F, to: G) = {
    val fromHelper = implicitly[FileLike[F]]
    val toHelper = implicitly[FileLike[F]]
    //...
  }
  def synchronizeExpl[F, G](from: F, to: G)(implicit fromHelper: FileLike[F], toHelper: FileLike[G]) = ???
  synchronize(new File("foo"), new File("bar")) //whohooo
  synchronizeExpl(new File("foo"), new File("bar")) //wowo EXPL
  //synchronize(2, 3) of course, no implicits ...
  
  //The infinite benefits of typeclasses:
  //-SoA (separation of abstractions): a type class allows to define new abstractions on top of existing arbitrary types
  //-composability: they can easily be plugged into methods, and different typeclasses on different type instances on the mix
  //-overridable: being based on implicits, a user can decide to override the behavior provided in the typeclass object
  // for the type by just placing an implicit with an higher priority (ie in a scope that gets read before, in the
  // lookup chain)
  //-typesafe!: if we miss the implicits (as above), compile time error, not runtime blowup as with reflection (eg
  // asInstanceOf's)
  
  //We can self-reference inside a type class:
  trait Serializable[T] {
    def serialize(t: T): Array[Byte]
  }
  object Serializable {
	  implicit def tuple2[T,V](implicit t : Serializable[T], v : Serializable[V]) = new Serializable[(T,V)] {
	    def serialize(tv: (T, V)) = t.serialize(tv._1) ++ v.serialize(tv._2)
	  }
  }
  
  //Pure type level reification:
  sealed trait TBool { type If[TrueType <: Up, FalseType <: Up, Up] <: Up }
  //the parametric type If is basically a method in the type world; we can then define:
  class TTrue extends TBool { type If[TrueType <: Up, FalseType <: Up, Up] = TrueType }
  class TFalse extends TBool { type If[TrueType <: Up, FalseType <: Up, Up] = FalseType }
  
  type Z[T <: TBool] = T#If[String, Int, Any]
  //we accept any TBool but we define here already its If type-method; now basically If returns a type; for TTrue
  //instanced like this we'll have String, for TFalse Int so that:
  type Ztrue = Z[TTrue]
  type Zfalse = Z[TFalse]
  val s: Ztrue = "baba"
  val t: Zfalse = 1
  
  //natural numbers!
  sealed trait Nat
  object Nat {
	  sealed trait _0 extends Nat
	  sealed trait Succ[Prev <: Nat] extends Nat
	  type _1 = Succ[_0]
	  type _2 = Succ[_1]
		
  }
}

object Scalajava {
  //In Java we have a difference between primitives and objects:
  //-primitives are values of a basic type like int, double etc, and they are passed by value
  //-objects are class instances and get passed by ref; they live in the heap
  //For every primitive type there exists a class whose instances box it, and Java provides implicit conversions to
  //allow to work seamlessly (since 5); Scala makes life even easier with implicit conversions:
  val x: Integer = 2
  val y: Int = new Integer(2)
  //In Scala there is no distinction between the two anyway, everything is an object to the programmer, but of course
  //whenever possible the bytecode refers to primitive values. We can explicitly ask the compiler to avoid (un)boxing
  //with an annotation:
  trait Iterator[@specialized(Int) T] { def hasNext: Boolean; def next: T }
  //if T=Int then a bytecode method returning a value of type Int is returned, not a generic type Object due to erasure.
  //@unspecialized exists too
  //Conversion usecase: lists
  val javalist: java.util.List[Integer] = new java.util.ArrayList[Integer]()
  val naughtyScalalist: List[Int] = javalist.asInstanceOf[List[Int]] 
  //this is OK, bytecode is the very same; putting it as an implicit conversion is safer though
  
  //Java and Scala visibilities are not compatible:
  class Test { protected val x = 10 } //Scala
//  class Test2 {
//	  public static void main(String[] args) {
//	    Test obj = new Test();
//	    System.out.println(obj.x());
//	  }
//  } //Java; access is successful!
  
  //Since of course a Scala class can be extended in Java and vice versa, a known integration pattern is to split Scala
  //and Java behavior in two different entities: a Scala abstract class, and a Java concrete class extending it.
  
  //Safetip: implicit views across the Scala-Java border are not good, and can lead to unexpected behavior (eg missed
  //equalities).
  
  //When using asJava and asScala conversions: the parameter type of the List needs to be converted too!

  //Scala serialization for certain entities can be troublesome; for example it is better to materialize the following:
  trait X extends java.io.Serializable
  class Y
	object Foo {
	  def test1 = new X { def foo = "HI" }
	  def test2 = new Y with X
	  def test3 = List(1,2,3).map(_.toString)
	}
  //into:
  class One extends X { def foo = "HI" }
  class Two extends Y with X
	object FooMat {
	  def test1 = new One
	  def test2 = new Two
	  def test3 = List(1,2,3).map(_.toString)
	}
  //because the serialization for anonymous classes, which are created for the methods above, is dependent on the
  //contents of the container iself, Foo.
  
  //A tour of Scala annotations (check subclasses of Annotation for the full list):
  import beans._ //a shitload of annotations to make classes compliant with the JavaBeans specification 
  
  @deprecated("This method is deprecated", "1.5")
  def thisMethodIsDeprecated() = ???
  
  def thisMethodHasADeprecatedArgname(x: Int, @deprecatedName('y) n: Int) = ???
  thisMethodHasADeprecatedArgname(1, y = 5) //this will give the deprecation warning
  
  @elidable(0)
  def log(msg: String) = println(msg)
  class Debuggy() { while(true) log(util.Random.nextString(4)) }
  //If this code is compiled with -Xelide-below <arg> with <arg> greater than 0 nothing will be printed
  
  @inline
  def fastmult(x: Int, y: Int) = x * y
  //This inlines the method, if possible; the JVM is able to figure out when to inline itself so most of the times this
  //is not needed (a @noinline exists too)
  
  @native
  def someWeirdC(c: Char): Int
  // See Swig generated code for example
  
  @remote trait Hello { def sayHello(): String }
  //RMI & Friends; this is equivalent to:
  trait Helloz extends java.rmi.Remote { @throws[java.rmi.RemoteException] def sayHello(): String }
  
  //@specialized
  //see above
  
  @strictfp def chirurgicFloats(f: Float) = f + 0.42f
  //This makes the Float and Double operations stick to the IEEE-754 standard for these numeric formats, even if extra
  //precision might be available; this is the choice if we need this code to produce the very same results on every
  //compliant machine
  
  val Constant = 'Q'
  def tokenMe(ch: Char) = (ch: @switch) match {
	  case ' ' | '\t' | '\n'  => 1
	  case 'A' | 'Z' | '$'    => 2
	  case '5' | Constant     => 3
	  case _                  => 4
  }
  //Compilation does not pass if the match can't be optimized into a Java switch; as long as only literals are present
  //the transformation may take place (here it fails since we have the variable Constant)
  
  def myIdealRecursion() = ???
  //Makes sure that method can be made into a tail recursive stack loop; compilation fails if the method is impossible
  //to convert
  
  //@unchecked prevents from additional compiler checks on the code; from the ScalaDoc:
  object Test extends App {
	// This would normally warn "match is not exhaustive" because `None` is not covered.
	def f(x: Option[String]) = (x: @unchecked) match { case Some(y) => y }
	// This would normally warn "type pattern is unchecked" but here will blindly cast the head element to String.
	def g(xs: Any) = xs match { case x: List[String @unchecked] => x.head }
	
	//@throws supra
	
	class SemiSeri extends Serializable {
	  val ciccio = 4
	  @transient val sessionId = util.Random.nextString(4)
	}
	//a @transient field is never serialized

  }
  
  object ABitOfFP {
    //Category theory for the (very) impatient:
    //concepts <=> data types
    //arrows (morphisms) <=> conversions between types (change of a value in a category to another in the same category)
    //CONCEPTS+ARROWS=CATEGORY
    //collections <=> containers (eg List), associated to functors; applying them, we stay in the category of types
    //functor: transformation from one category to another and morphism-preserving
    //A morphism sends cats to cats, a functor cats to dogs; a morphism transforms an Int into a String, and an Option
    //is a functor since if I apply this arrow on a Option[Int] I get an Option[String] 
    //Applicative functor: tool that makes a single function out of many, both wrt inputs and outputs
    
    //Currying: transforming a function accepting a number of arguments into a function that accepts only a single
    //parameter and returns a function accepting the next parameter only (and so on...):
    //I have: f: AxB -> C I get g: A -> (B -> C)
    val kerry = (a: String, b: Int) => a+b.toString
    val kerryCurry = kerry.curried
    kerry("a", 1)
    kerryCurry("a")(1)
    
    //The Applicative Style:
    //build1(input) @ build2(input) => MyClass(_, _) (you can do stuff like this in Scalaz)
    //Another important pattern: monadic workflows (for my ScalaZee days too)
    
  }
  
  object DependentTypes {
    
    trait DepValue {
      type V
      val value: V
    }
    def magic(that: DepValue): that.V = that.value
    
    class IntDepValue extends DepValue {
      type V = Int
      val value = 1
    }
    val v: Int = magic(new IntDepValue)

    //Basically the return type of magic depends on the argument we pass in, on the V from DepValue.

  }
  
}


