import annotation.tailrec
import util.Random
import async.Async.{async, await}
import collection.JavaConverters.mapAsScalaMapConverter
import concurrent._
import ExecutionContext.Implicits.global
import concurrent.duration._
import language.experimental.macros
import reflect.api.StandardNames
import reflect.macros.Context
import reflect.runtime.universe
import reflect.runtime.universe._
import reflect.runtime.universe.TypeTag.Int
import tools.reflect.ToolBox
import runtime.ZippedTraversable2.zippedTraversable2ToTraversable
import java.net.InetSocketAddress
import akka.actor._
import akka.io._
import akka.io.{IO => AIO}
import akka.util.ByteString
import shapeless._
import shapeless.{Lens => SLens}
import shapeless.Poly1
import shapeless.Poly.inst1
import shapeless.PolyDefns.Case

object ScalaInAction {
  //funny lambdas with closures - it doesn't matter that's an object method; the _, not required, does the trick: it
  //instructs to take the method as an object, not to evaluate it
  class Adder(offset: Int) {
    def add(n: Int) = offset + n
  }
  val add10 = new Adder(10)
  List(1, 2, 3, 4, 5).map(add10.add _)
}

object ScalAsync {
  
  //The async macro marks a non-blocking block of code; the await one suspends the computation waiting for the future 
  //to complete. 
  //In the first snippet, we define a function returning a future and an int; the async block is executed in
  //another thread, ie asynchronously, so the control is passed to the print immediately; the printed value is zero 
  //since the assignment is inside the sync block; in here the two futures are waited for with the await call.
  //In the second, we throw two subsequent asynchronous computations, so they are in fact parallel, and wait an amount
  //of time exactly equal to what it takes for the future to complete; we are lucky because "service" times (eg forking
  //the thread) are a jiffy (the actual computation has an overall duration of 5+epsilon seconds). Clearly, if we 
  //computed two futures with different sleep times the timeout is dictated by the slower of the two.
  //The third is a variation of the second where the computation is sequencial instead; 5 secs is not enough! We would
  //have to launch the first, pass the control to +, which is an operator!!, launch the second and automatically "wait"
  
  def futurez = future{Thread.sleep(5000); 2}
  var o = 0  
  async {o = await(futurez) + await(futurez)}
  println(o)
  
  //---cut here---

  def fut1: Future[Int] = async{await(futurez)}
  def fut2: Future[Int] = async{await(futurez)+2}
  val zip = Await.result(fut1.zip(fut2), 5.seconds)
  
  //---cut here---
  def futfut: Future[Int] = async{await(futurez) + await(futurez)}
  val wait4 = Await.result(futfut, 5.seconds)
  
}

object Polymorphism {
  
  def headOption[T](l: List[T]) = l.headOption 
  val IntHeadOption: List[Int] => Option[Int] = headOption _ //exact type is inferred! _ optional
  val NothingHeadOption = headOption _ //the lowest possible type is inferred! _ mandatory
  val StringHeadOption = headOption[String] _ //always possible
  
  object headOptionPoly extends Poly1 {
    implicit def caseT[T] = at[List[T]](_.headOption)
  }
  //headOptionPoly(List(1)); headOptionPoly(List("")) //macro-based, may not compile
  val hop = headOptionPoly
  //the relevant thing is that hop is actually a value here, whereas we cannot give to map an head option other than
  //Any or Nothing-based, by losing type information and the functionality along
  //val trans = List(List(1), List(""), List(true), List(Nil)).map(hop) macro-based, may not compile
  val transAny = List(List(1), List(""), List(true), List(Nil)).map(headOption[Any])
  val transNothing = List(List(1), List(""), List(true), List(Nil)).map(headOption _)
  //val transInt = List(List(1), List(""), List(true), List(Nil)).map(headOption[Int]) //error!
  //transNothing.head.get+1 not good!
  //sampling our function signal at a particular type:
  val intHeadOptionPoly = headOptionPoly.caseT[Int]
  //a proper function val is returned
  val transInt = List(List(1), List(2), List(3)).map(intHeadOptionPoly)
  transInt.head.get + 1
}

object ScalaPuzzlers {
  {
    def objFromJava: Object = "string"
    def stringFromJava: String = null
    
    def printLengthIfString(a: AnyRef) {
      a match {
        case s: String => println("String of length " + s.length)
        case _ => println("not a string")
      }
    }
    
    printLengthIfString(objFromJava)
    printLengthIfString(stringFromJava)
  
    //This prints out:
    //	String of length 6
    //	Not a string
    //because:
    //The literal from objFromJava is a valid String, returning it as an Object we lose String methods clearly, but 
    //since it is a valid String literal, pattern matching can pull out that type.
    //null is not an instance of String. This is inherited from Java; the proper way for empty strings is
    //Option[String]; we can always have 'case null' in order to catch the case. Remember that strings in C are
    //arrays of char, we are just assigning a null pointer here, not a string literal.
  }
  
  {
    val ints = List(1, 2, 3)
    val accumulator: List[Int] = Nil
    
    val leftReversed = (ints foldLeft accumulator){ (acc, elem) => elem :: acc }
    val rightReversed = (ints foldRight accumulator){ (elem, acc) => acc :+ elem }
    println(leftReversed)
    println(rightReversed)
    
    println((leftReversed /: accumulator) { (acc, elem) => elem :: acc })
    println((rightReversed :\ accumulator) { (elem, acc) => acc :+ elem })

    //List(3, 2, 1) because 3 :: (2 :: (1 :: nil))
    //List(3, 2, 1) because ((nil :+ 3) :+ 2) :+ 1 where :+ is appension 
    //List(3, 2, 1) because /: is alternate syntax for foldLeft, z :\ xs is the same as xs foldLeft z; so here we 
    // have acc = (3, 2, 1) but no elem, because accumulator is the empty list
    //List(1, 2, 3) because :\ is alternate syntax for foldRight, xs :\ z is the same as xs foldRight z; 
    // here it's ((nil :+ 1) :+ 2) :+ 3
  }
  
  {
    val (x, y) = (List(1, 3, 5), List(2, 4, 6)).zipped find (_._1 > 10) getOrElse (10)
//    println(s"Found $x")
    
    //This shows a runtime error. At compile time this is ok because (x, y) comes with no type and getOrElse can
    //return an arbitrary one. At runtime, a pattern matching is carried out to assign a value to the tuples but 10
    //does not match (x, y).
  }
  
  {
    class A {
      type X //the top of the type poset is Any, so for an arbitrary type X, X <: Any
      var x: X = _ //this initializes to the default value for the type; you can do this only with var's, not val's!
    }
    
    class B extends A {
      type X = Int
    }
    
    val b = new B
    println(b.x)
    
    val bX = b.x
    println(bX)
    
    //Prints null and 0. x is not explicitly initialized in the subclass, and println accepts any type, so no
	  //conversion takes place, and null is printed because this is default value for the type Any from the superclass.
	  //But what is really weird, is that the assignment triggers the unboxing (removing the container type to leave the
	  //actual). Always remember that until you really state the type, the current is likely to be the general.
  }
  
  {
    
    def fromJava: java.util.Map[String, java.lang.Integer] = {
      val map = new java.util.HashMap[String, java.lang.Integer]()
      map.put("key", null)
      map
    }
    
    val map = fromJava.asScala.asInstanceOf[scala.collection.Map[String, Int]]
    
    println(map("key") == null)
    println(map("key") == 0)
    
    //Scala Int and Java Integer are not quite the same; for example, null is not an accepted value for the former
    //but it is for the other. Scala is able to convert from and to (eg BoxesRunTime.unboxToInt) and nulls are sent
    //to 0. So for the second println it is a clear true, since null was replaced with 0. For the first one, the
    //rule of "comparing a null and and Int will always yield false" does not apply, it seems because the runtime
    //(not the compiler, since this "will always be false" warning is shown) is able to say that we are comparing 
    //that null with a map to an Int value, and transform it into a 0.
  }
  
  {
    implicit class Padder(val sb: StringBuilder) {
      def pad2(width: Int) = {
        1 to width - sb.length foreach { sb append '*'}
        sb
      }
    }
        
    val greeting = new StringBuilder("Hello, kitteh!")
    println(greeting pad2 20)
    
    val farewell = new StringBuilder("Bye now.")
    println(farewell pad2 20)
    
    //Implicit classes: we use them to pimp a type, here we add a pad2 method to StringBuilder; they can be defined
    //only inside another class/trait/object, and they can have only one regular parameter (the type we act on). The
    //result is "Hello, kitteh!*" and a runtime exception. In the method, what actually happens is that a single * is
    //appended, and then the apply() of StringBuilder, which is charAt, is called on each char. For the hello, it is
    //20-14=6 times, so we are not going out of bounds, but for the other we are.
  }
  
  {
    var x = 0
    def counter = {x += 1; x}
    def add(a: Int)(b: Int) = a + b
    val adder1 = add(counter)(_)
    val adder2 = add(counter) _
    
    println("x = " + x + ", a1= " + adder1(10))
    println("x = " + x + ", a2= " + adder2(10))
    println("x = " + x)

    //(_) represents an anonymous function, _ an eta-expansion: the only actual difference is that in the first case
    //args eval is lazy, in the other eager. This is the reason why x gets incremented in adder2, not in adder1. The
    //first print will be (1, 12): x=0 was incremented and adder1 computes add(1+1)(10), calling counter again which
    //sets x=2; the second (2, 11): now x=2, but adder2 had already evaluated everything at its definition (eager), 
    //so that what it computes is 1+10; the last is 2 because adder2 did not update the variable in the previous step
  }
  
  {
    trait A {
      val foo: Int
      val bar = 10
      println("In A: foo: " + foo + " bar: " + bar)
    }
    class B extends A {
      val foo: Int = 25
      println("In B: foo: " + foo + " bar: " + bar)
    }
    class C extends B {
      override val bar = 99
      println("In C: foo: " + foo + " bar: " + bar)
    }
    new C
    
    //This prints (0, 0), (25, 0) and (25, 99) for A, B and C respectively; the constructors are called in this order.
    //For foo, the first is 0 since the value is abstract and therefore defaulted. For bar, we want it to be 99 in C,
    //and it is a val, vals can be initialized only once, so the compiler just ignores the previous assignments, and
    //the default value for the inferred type (Int, because in A we have bar = 10) is left.
  }
  {

    
    case class Foo[T: TypeTag](x: T) {
      val tpe = typeOf[T]
    }
    val typeOfFoo = Foo(0).tpe
    //An actual reflection Type, access to more information than the Class(Tag) (see below) can bring
    
    val membersOfFoo = typeOfFoo.members
    //An entity such as a class comprises different other elements (constructors, values, types...), and each of them
    //is referred to by a symbol (reflect.runtime.universe.Symbol)
    
    val equals = typeOfFoo.member(newTermName("equals"))
    val nonexistingMethod = typeOfFoo.member(newTermName("bar"))
    //We can look up symbols
    
    val myOpsTree = reify {
      object MyOps { 
        def add(a: Int, b: Int) = a + b
      }
    }.tree
    //The AST of the great MyOps object
    
    val myOpsTreeRaw = showRaw(myOpsTree)
    //Raw version, for macro use
    
    case class A[T: Manifest](t: T) { def m = manifest[T] }
    val stringManifest = A("a").m
    //type retrieval before 2.10

    //extracting types from a map
    val typesList = A(Map.empty[String, Int]).m.typeArguments
    //pre 2.10
    
    val structInfo = Foo(Map.empty[String, Int]).tpe.erasure
    structInfo match { case TypeRef(_, _, args) => args }
    //post 2.10
    
  }
  
  object Lenses {
    
    //Java-style, referring to nested fields comes easy:
    class A {
      class B {
        class C(var d: Int)
        val c: C = new C(42)
      }
      val b: B = new B
    }
    val a = new A() //{ val b = new B{ val c = new C(1) } }
    a.b.c.d = 1
    
    //Scala-style requires case classes and their copy methods:
    case class Z(val w: Int)
    case class Y(val z: Z)
    case class X(val y: Y)
    
    val x = X(Y(Z(1)))
    x.copy(
      y = x.y.copy(
        z = x.y.z.copy(
          4
        )
      )
    )
    //because "immutability comes at a price", which can be discounted with lenses.
    //Lens prototype:
    case class Lens[O, V](
      get: O => V,
      set: (O, V) => O 
    )
    //where O stands for "object" and V for "value", get returns the value v: V of o: O and set returns a new o_nu: O 
    //instance by updating the argument o: O with a value v: V
    def getX(x: X) = x.y
    def setX(x: X, yNu: Y) = x.copy(y = yNu)
    val xLens = Lens(getX, setX)
    
    //shapeless lenses: easily refer to nested values by indexing; indexes are checked (>> 5 will not compile)
    //val xYLens = lens[X] >> 'y macro-based, may not compile
    //val xYZLens = xYLens >> 'z
    //val xYZwLens = xYZLens >> 'w 
    
    //types are inferred
    //val xY: Y = xYLens.get(x)
    //val xyZ: Z = xYZLens.get(x)
    //val xyzW: Int = xYZwLens.get(x)
    
    //a fresh w value at the core of x 
    //val xNu = xYZwLens.set(x)(4)
    
  }
  
  object Sudoku {
    //A Sudoku solver. In the Sudoku game, we are given a 3x3 grid whose cells are in turn 3x3 grids, and each little
    //cell may or may not have a number ranging from 1 to 9 (at least 20 of them have one); the aim of the game is to
    //fill the cells as to have no repeated number in every row, column and region.
    
    object Domain {
      type Cell = Option[Int]
      type Table = IndexedSeq[IndexedSeq[Cell]]
      val dim = 9
      val vals: Set[Cell] = (1 to 9).map(Some(_)).toSet
      implicit class RichTypeInfo(table: Table) {
		    def write = table.map(_.toString).mkString("\n")
      }
    }
    import Domain._
    
    //The grid:
    case class Grid(
      table: Table = IndexedSeq.fill[Cell](dim, dim)(None),
      boxWidth: Int = dim, 
      boxHeight: Int = dim
    ) {
      val size = boxWidth * boxHeight
      val boxesInWidth = size/boxWidth
      val boxesInHeight = size/boxHeight
      
      def updated(x: Int, y: Int, value: Cell) = {
        new Grid(table.updated(y, table(y).updated(x, value)), boxWidth, boxHeight)
      }
      
      def apply(x: Int, y: Int) = table(y)(x)
      
      def column(x: Int) = for(i <- 0 until boxHeight) yield table(i)(x)
      
      def row(y: Int) = table(y)
      
      //the block containing the cell, represented by its coordinates on the grid
      def blockFor(x: Int, y: Int) = {
        def blockPoint(z: Int, steps: Int): Int = 
          if (z < 2)
            steps-1
          else
            blockPoint(z-2, steps+1)
        (blockPoint(x, 0), blockPoint(y, 0))
      }
      
      //the cells belonging to the block
      def block(m: Int, n: Int) = {
        val m3 = m*3
        val mBase = List(m3, m3+1, m3+2)
        val n3 = n*3
        val nBase = List(n3, n3+1, n3+2)
        (for (i <- mBase; j <- nBase) yield table(i)(j)).toIndexedSeq
      }
      
      //all of the allowed values for the cell; we can compute this by complementing the union of column, row and
      //blockFor for the cell
      def possibleValues(x: Int, y: Int): Set[Cell] = {
        val columnSet = column(x).toSet
        val rowSet = row(y).toSet
        val (blockPointX, blockPointY) = blockFor(x, y)
        val blockSet = block(blockPointX, blockPointY).toSet
        val unionSet = columnSet ++ rowSet ++ blockSet
        vals -- unionSet
      }
      
	    def isSolved = {
        val rows = for (m <- (0 until boxHeight).view) yield row(m)
        val columns = for (n <- (0 until boxWidth).view) yield column(n)
        val blocks = for (m <- (0 until boxHeight).view; n <- (0 to boxWidth).view) yield block(m, n).toIndexedSeq
        rows.forall(_.toSet == vals) && columns.forall(_.toSet == vals) && blocks.forall(_.toSet == vals)
	    }
	    
    }
    
    @tailrec
    def fill(data: Grid): Grid = {
      val possibleCoordinates = for (x <- 0 until data.size; y <- 0 until data.size) yield (x, y)
      val coordinatesToFill = 
        possibleCoordinates.view.map{
          case (x, y) => (x, y, data.possibleValues(x, y))
        }.collectFirst{
          case (x, y, possibleValues) if (possibleValues.size == 1) => (x, y, possibleValues.head)
        }
      coordinatesToFill match {
        case Some((x, y, value: Cell)) => {
          fill(data.updated(x, y, value))
        }
        case None => data
      }
    }
    
    //SDCI - SPATAFFIATA DI CODICE ILLEGGIBILE
    def init(grid: Grid) = {
      val n = Random.nextInt(15) + 20
      def update(
        table: Table, 
        pos: (Int, Int), 
        vals: Set[Cell], 
        sets: (IndexedSeq[Cell], IndexedSeq[Cell], IndexedSeq[Cell])
      ): Table = 
        if (vals.isEmpty)
          table
        else {
          val curr = vals.head
          if (!sets._1.contains(curr) && !sets._2.contains(curr) && !sets._3.contains(curr))
            table.updated(pos._2, table(pos._2).updated(pos._1, curr))
          else
            update(table, pos, vals.tail, sets)
        }
      def initHelper(table: Table, pos: (Int, Int), toGo: Int): Table = 
        if (toGo == 0)
          table
        else 
          if (pos == (8, 8))
            initHelper(table, (0, 0), toGo)
          else {
            val nextPos = 
              if (pos._1 == pos._2 || pos._1 < pos._2)
                (pos._1+1, pos._2)
              else
                (pos._1, pos._2+1)
            if (Random.nextBoolean == true) {
              val column = grid.column(pos._1)
              val row = grid.row(pos._2)
              val block = grid.block(pos._1, pos._2)
              val updatedTable = update(table, pos, vals, (column, row, block))
              if (table != updatedTable) 
                initHelper(updatedTable, nextPos, toGo-1)
              else
                initHelper(updatedTable, nextPos, toGo)
            }
            else
              initHelper(table, nextPos, toGo)
          }
      initHelper(grid.table, (0, 0), n)
    }
    
    def guess(data: Grid): Seq[Grid] = {
      val allCoordinates = for (x <- 0 until data.size; y <- 0 until data.size) yield (x, y)
      val Some((x, y)) = allCoordinates.collectFirst{
        case c@(x, y) if (!data(x, y).isDefined) => c
      }
      val possibilities = data.possibleValues(x, y)
      possibilities.map{ p => data.updated(x, y, p)}.toSeq
    }
    
    def solve(data: Grid): Option[Grid] = {
      val filledIn = fill(data)
      if (filledIn.isSolved) 
        Some(filledIn)
      else
        guess(filledIn).view.map(solve).collectFirst{case Some(grid) => grid}
    }

  }
  
  object AkkaIO {
    
    //Akka IO is based on Java selectors, the implementation of a dispatcher thread blocking on data wait and
    //assigning the work units to the, obviously nonblocking, threads of a threadpool. Here, the blocking dispatching
    //is hidden to the users, and it is performed by the so named "selection handlers", managed by a "manager" coming
    //from the IO extension. The channel actors map to the working threads, receiving events from the handlers.
    

    
    implicit val as = ActorSystem("LE NETINET")
    val isa = new InetSocketAddress("localhost", 31337)
    
    class SimpleSender(remote: InetSocketAddress)(implicit val as: ActorSystem) extends Actor {
      
      AIO(Udp) ! Udp.SimpleSender //Acquire a manager from the extension; syntax is IO(<proto>)

      def receive = {
        case Udp.SimpleSenderReady => context.become(ready(sender)) //ack for it, act like ready now
      }
      
      def ready(socket: ActorRef): Receive = {
        case msg: String => 
          socket ! Udp.Send(ByteString(msg), remote)
          sender ! Ack
      } 
      
      new SimpleSender(isa)
      
    } 
    
    class SimpleReceiver(implicit val as: ActorSystem) extends Actor {
      
      AIO(Udp) ! Udp.Bind(self, isa)
      
      def receive = {
        case Udp.Bound(local) => context.become(ready(sender))
      }
      
      def ready(socket: ActorRef): Receive = {
        case Udp.Received(data, remote) => println(data)
      }
      
    }
    
    //In any async io system the backpressure issue must be coped with: a slow receiver can't keep up with a fast
    //sender. Different options here:
    
    //ack-based flow control; the SimpleSenderSupervisor is itself managed
    
    case class SendIt(data: String)
    case object Ack
    case object Shutdown
    
    class SimpleSenderSupervisor(connection: SimpleSender)(implicit val as: ActorSystem) extends Actor {
      
      private def buffer(data: String) = ??? //add the data chunk to the buffer
      
      def receive = {
        
        case SendIt(data) => 
          buffer(data)
	        connection.self ! data
	        context.become({
	          case SendIt(data) => buffer(data)
	          case Ack => //start serving from buffer...
	        }, discardOld = false) //the new behavior is pushed onto the previous
	        
        case Shutdown => context stop self
      
      }
    }
    
    //nack (negative ack)-based: whenever the receiver cannot process a new data unit, it returns a negative ack; for
    //each unit on which the sender gets back a nack, it stores that in a fifo; the receiver cycle is: get new data
    //with a SendData; if the queue is empty, send it immediately, if it is not enqueue that and try and pick the head
    //of the queue
    
    
    //Pipeline actors: an abstraction for composing event and command handling. Typed on the actor system context, 
    //commands from above and below, events from above and below. Each stage is a port forwarding commands to execute
    //and events to handle. Pipelines live in actors only and performed on a single thread.
    class MyStage[A <: akka.io.PipelineContext, B, C, D, E] extends PipelineStage[A, B, C, D, E] {
      def apply(ctx: A): akka.io.PipePair[B,C,D,E] = ??? 
    }
    
  }
  
  object MacroParadise {
    

    
    object Config {
      val loggingEnabled = Random.nextBoolean
      class Logger() {
        def log(severity: Int, message: String) = println(s"$message (severity: $severity)")
      }
      val logger = new Logger()
    }
    
    //A macro is basically a method stub, the implementation is somewhere else...
    def log(severity: Int, message: String): Unit = macro impl
    
    //In the implementation, the parameters come as AST's, formal representation of some Scala code; the result of a
    //macro evaluation is an AST too. Macro bundles allow for looking up a context automatically
    def impl(c: Context)
      (severity: c.Expr[Int], message: c.Expr[String]): c.Expr[Unit] = {
        import c.universe._
        reify {
          if (Config.loggingEnabled)
            Config.logger.log(severity.splice, message.splice)
        }
      }
    
    //reification works only if types are known; the following won't compile:
//    def foo(xs: Any*) = ...
//    val args: List[Expr[Any]] = ...
//    reify{ foo(args.splice) }
    
    //type and generic macros: for a strongly-typed db wrapping (compiles with Scala paradise)
//    type H2DB(url: String) = macro impl
//    
//    def impl(c: Context)(url: c.Tree) = {
//      //the code generated by generateCode encapsulates the database
//      val wrapper = q"trait Wrapper { ${generateCode(url)} }"
//      //publishing the object so that it is visible in the whole program
//      val wrapperRef = c.introduceTopLevel(wrappersPkg, wrapper)
//      //this macro expansion yields a ref to wrapperRef
//      q"$wrapperRef($url)"
//      ...
//    }
//    
    //Another example:
    val tree = Apply(Select(Literal(Constant(40)), newTermName("$plus")), List(Literal(Constant(2))))
		val cm = runtimeMirror(getClass.getClassLoader)
		val tb = cm.mkToolBox()
    tb.eval(tree)
  }
  
  object ConfessionsOfARubyDeveloper {
    
    //Two main ways to add native functionality to types:
    
    //implicit classes; notice that methods already there in the type cannot be overriden (eg toString); ok for
    //extending with simple functionality and tied to a particular type; the "extends AnyVal" part is just an 
    //optimization telling the compiler to avoid instantiating the FutureCompanionOps object where possible and call 
    //its methods directly.
    implicit class RichInt(val i: Int) extends AnyVal {
      def toBits = i*8
    }
    2.toBits
    
    //type classes; we have the so called type class:
    trait StorageUnitConversions[A] {
      def toBits(a: A): A //if the value is large enough, * 8 could not fit in the given type, but let's ignore possible 
                          //overflows...
    }
    //the default impl:
    object StorageUnitConversions {
      implicit object IntToBits extends StorageUnitConversions[Int] {
        def toBits(i: Int) = i*8
      }
    }
    //its use:
    object Example {
      def convert[A](a: A)(implicit convertor: StorageUnitConversions[A]) {
        convertor.toBits(a)
      }
    }
    Example.convert(2)
    
    //Stackable traits:
    //a base trait:
    trait Base {
      def base(d: Double) = ???
    }
    //two specific traits:
    trait ABase extends Base {
      abstract override def base(d: Double) = {
        //... perform A-specific computation
        super.base(0)
      } 
    }
    trait BBase extends Base {
      abstract override def base(d: Double) = {
        //... perform B-specific computation
        super.base(1)
      } 
    }
    //using:
    class TheBase[B] extends Base with BBase {
      base(3)
    }
  }
}
  
