package all.about.scala

import beans.BeanProperty
import collection.JavaConverters._
import language.dynamics
import java.util.concurrent._
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.net.{Socket, ServerSocket}

object TwitterScalaSchool {
	
  object Variance {
    //If B is a subtype of A, then what about the relation between F[B] and F[A], where F is a functor (eg List)?
    //
    //if B <: A and	F[B] <: F[A] then A is covariant, +A
    //								F[A] <: F[B] then A is contravariant, -A
    //								F[A] || F[B] then A is invariant, A
    //
    //By the Liskov substitution principle, subtyping implies substitutability
    
	  class Covariant[A]
    //this does not compile, we would need +A instead
    //val cPlus: Covariant[AnyRef] = new Covariant[String]
	  //this -A
    //val cMinus: Covariant[String] = new Covariant[AnyRef]
	  
	  //We are good with A and +A but what about -A? A simple use case, remembering that in Scala we have:
	  //trait Function1 [-T1, +R] extends AnyRef
	  
	  class Animal { val sound = "rustle" }
	  class Bird extends Animal { override val sound = "call" }
	  class Chicken extends Bird { override val sound = "cluck" }
	  
	  //Function arguments are contravariant: Bird <: Animal and f: Animal => String <: f: Bird => String (it filz like
	  //we are breaking the constraint wow!)
	  val getTweet: (Bird => String) = ((a: Animal) => a.sound )
	  
	  //Returns are covariant instead: it's ok to return a Chicken because we're asked a Bird, a chicken is a bird :)
	  val hatch: (() => Bird) = (() => new Chicken )
	  
  }
  
  object Evidence {
    
    def id[A](a: A): A = a //stanard polymorphism
    
    def id1[B, A <: B](b: B) = b //upper bound
    def id2[B, A >: B](b: B) = b //lower bound
    
    def id(a: List[_]) = a //dontcare, shorthand for:
    def idv(a: List[T forSome { type T }]) = a
    
    //In CiccioMimmoVerbose we reference to the type Ciccio carried by an instance of type Mimmo
    trait Mimmo { type Ciccio = Int; val assss: Double }
    type CiccioMimmo = Mimmo#Ciccio
    type CiccioMimmoVerbose = x.Ciccio forSome { val x: Mimmo }
    def cm(woo: CiccioMimmo) = woo
    def cmv(waa: CiccioMimmoVerbose) = waa
    cm(1)
    cmv(1)
    
    def idd(a: List[_ <: Int]) = a //wild bound
    
    //With:
    def foo[A](a: A)(implicit ma: List[A]) = a
    implicit val ma = List(1)
    foo(1)
    foo("42")(List("aaa"))
    //we require a value of type List[A] to be in the implicit scope (or it can be passed directly of course too). It
    //can be rewritten more coincisely as:
    def bar[A : List](a: A) = println(implicitly[List[A]].length)
    //and since it is anonymous we can retrieve it through the implicitly method; this style is called "implicit evidence"
    
    //Scala has rank-1 polymorphism, which means that no type variable can be free; it is also referred to as prenex
    //polymorphism because the types indication [A, B] corresponds to the predicative formula:
    //forall(A)forall(B)args(A, B)
    //where args(A, B) = (f: A => List[A], b: B), and this is a prenex formula, one where a string of quantifiers
    //precedes one of quantifier-free formulas.
    //In the following therefore, both A and B are fixed, and different, therefore the following will not compile:
    //def foo[A, B](f: A => List[A], b: B) = f(b)
    //because A = B is what we would ask for here. We can't say that we accept an argument of arbitrary type B and we
    //process it with a function that may work with an arbitrary type, B included. These are as said type variables
    //not type schemas.
    
    //All these were example of parametric polymorphism: we use variable types for actual types but the code behaves
    //the same.

    //View bounds, the Scala way for type classes: it is a system to specify that an arbitrary class can be viewed as
    //another, and thus supporting all of the methods of this destination class. The type class is the set of all the 
    //types for which this view map is defined. This machinery relies on implicits:
    Stream(1,2).+:(7)
    implicit def intToStr(i: Int) = i.toString
    implicit def floatToStr(f: Float) = f.toString
    
    class Container[A <% String] { def concatIt(x: A) = "42" + x }
    new Container[String].concatIt("42") //of coursa
    new Container[Int].concatIt(42)
    new Container[Float].concatIt(42F)
    //new Container[Double].concatIt(42) no view found! infact:
    //implicitly[Double => String] wont compile
    //whereas:
    implicitly[Int => String]
    implicitly[Float => String]
    //yes
    
    //With usual parametric typing a type expression has a single degree of freedom, like List[A], but Scala lets us
    //have more:
    trait AbstractContainer[M[_]] { def put[A](x: A): M[A]; def get[A](m: M[A]): A }
    val listContainer = new AbstractContainer[List] { def put[A](x: A) = List(x); def get[A](m: List[A]) = m.head }
    val optionContainer = new AbstractContainer[Option] { def put[A](x: A) = Some(x); def get[A](m: Option[A]) = m.get } 

    //The ability to write generic code on higher-kinded types is called ad-hoc polymorphism and implicits empower us
    //again in this
    implicit val implicitListContainer = listContainer
    implicit val implicitOptionContainer = optionContainer
    def tupleize[M[_]: AbstractContainer, A, B](fst: M[A], snd: M[B]) = {
      val c = implicitly[AbstractContainer[M]]                             
      c.put(c.get(fst), c.get(snd))
    }
    assert(tupleize(Option(1), Option(2)) == Some((1,2))) //A is automatically inferred of type (Int, Int)
    
    //Let us define:
    trait A { def f(other: A): A }
    //then:
    abstract class B extends A { def f(other: B): B = other }
    //will give a compile error because scalac does not recognize B.f as a legit implementation of A.f since the types
    //do not match in the f argument. With this code we could satisfy a pattern like: we want B's to be forced to
    //duplicate without losing their particular behavior of B's ie generalizing to A's.
    //To overcome this Scala supports F-bounded polymorphism:
    trait ABetter[T <: ABetter[T]] { def f(other: T): T  }
    class BBetter extends ABetter[BBetter] { def f(other: BBetter): BBetter = other }
    //or more understandably:
    trait AMuchBetter { type T <: AMuchBetter; def f(other: T): T }
    class BMuchBetter extends AMuchBetter { type T <: BMuchBetter; def f(other: T): T = sys.error("TODO") }
  }
  
  object Typeporn {
    
    trait T {
      //A type to be possibly defined in the extender:
      type Z
      //A type alias:
	    type Lol = List[Int]
	    //An abstract type:
	    type Asd <: IndexedSeq[Long]
	    //A structural type:
	    type Foo <: {
	      def bar: String
	    }
	    //Extenders of T are required to have an inner value baz with a pr0n method
	    val baz: {
	      def pr0n: Unit
	    }
    }
    
    //"Inline" structural types:
    def lol(x: {def asd: Int}) = x.asd
    
    trait Tee extends T {
      type Z = Int
      class Bazzz { def pr0n {} }
      val baz = new Bazzz
    }
    
    //You cannot instantiate a trait... well almost! Anonymous classes:
    trait A 
    abstract class B
    new A{}
    new B{}
    //We say "anonymous" because traits and abstract classes cannot be instantiated, and infact an invisible class
    //extending them is created and instantiated on our behalf. We can of course add method definitions.
  
  }
  
  object ConcurrencyALaTwitter {
    
    //Runnables and Callables
    class Runner extends Runnable with Callable[Int] {
      def run() { while(true) println("lol") }
      def call() = { Thread.sleep(5000); util.Random.nextInt }
    }
    //A Thread:
    new Thread(new Runner()).start() 
    //this blocks the main() thread! 
    
    //Another famous example: this program listens on a local port and returns the name of the thread it is executed
    //in, the main thread. It can serve only one client at a time, for example if I start two connections immediately
    //the second will not see "main" until the first is closed.
    import java.net.{Socket, ServerSocket}
    import java.util.Date
    import java.util.concurrent.{Executors, ExecutorService}

		class DummyNetworkService(port: Int) extends Runnable {
		  val serverSocket = new ServerSocket(port)
		
		  def run() {
		    while (true) {
		      //#2: This will block until a connection comes in, the accept() method is blocking
		      val socket = serverSocket.accept()
		      //#3: This here stays in the thread too of course
		      (new DummyHandler(socket)).run()
		    }
		  }
		}
		
		class DummyHandler(socket: Socket) extends Runnable {
		  def message = (Thread.currentThread.getName() + "\n").getBytes
		
		  def run() {
		    socket.getOutputStream.write(message)
		    socket.getOutputStream.close()
		  }
		}
		
		(new DummyNetworkService(2020)).run //#1: We are running this in the main thread
    
	  //Improvement. Replace:
	  
	  //(new Handler(socket)).run()
	  
	  //with:
	  
	  //(new Thread(new Handler(socket))).run()
	  
	  //This way multiple connections can be served together because each one is served in a new Thread. Possible issue:
	  //rapid growth of the number of threads.
	  
	  //As of Java 5 we have a new facility for thread management that for example allows to set the maximum number of
	  //workers:
	  
		import java.util.concurrent.{Executors, ExecutorService}
		
		class NetworkService(port: Int, poolSize: Int) extends Runnable {
		  val serverSocket = new ServerSocket(port)
		  //This thread pool will have a fixed number of workers, if any of them dies it is replaced. If new work is
		  //ready but no threads available new tasks will wait
		  val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
		
		  def run() {
		    try {
		      while (true) {
		        // This will block until a connection comes in - it is just the basic dummy above plus thread management
		        val socket = serverSocket.accept()
		        pool.execute(new Handler(socket))
		      }
		    } finally {
		      pool.shutdown()
		    }
		  }
		}
		
		class Handler(socket: Socket) extends Runnable {
		  def message = (Thread.currentThread.getName() + "\n").getBytes
		
		  def run() {
		    socket.getOutputStream.write(message)
		    socket.getOutputStream.close()
		  }
		}
	
	  (new NetworkService(2020, 2)).run
	  
	  //See TituliDiCoda for a discussion about thread synchronization in Java. Java 5 introduced goodies like:
	  
	  //CountDownLatch: a counter on which a thread blocks waiting for it to be 0. It serves the purpose of having a
	  //thread to wait for some other thread to be done, typically a worker but the main() thread too why not.
	  class Worker(startSignal: CountDownLatch, doneSignal: CountDownLatch) extends Runnable {
	    def run() {
	      try {
	        startSignal.await() //startSignal is meant to be a binary signal the threads wait on to start
	        Thread.sleep(50000) //this is the thread main activity
	        doneSignal.countDown()		//doneSignal is a counter set to N where N is the number of peer workers; the workers
	        													//decrease it to notify that their own job is done
	      } catch {case _: InterruptedException => println("Aborted")}
	    }
    }
	  
	  //ReadWriteLock: interface for handling separately read and write locks. This addresses the common pattern where
	  //it is ok for multiple workers to read a resource, but not to write it at the same time of course. A reader lock
	  //is always returned except when a write lock is on. Implementors need to take care of the lock scheduling, to
	  //avoid common problems as starvation.
	  //The implementation is ReentrantReadWriteLock, a lock manager that allows for reentrant locking, the same worker
	  //can reacquire a lock immediately after it releases it; this unfair mode, which is the default, allows for a
	  //faster execution but can of course delay an "unlocky" worker for a long time.
	  //Lock downgrading: a thread possesses a write lock, acquires a read lock too and finally releases the write.
	  //Allows for fast afterwrite access for cache-like structures. The rwl supports this but not the vice versa, up-
	  //grading. Example:
	  abstract class CachedData() {
    var data: Object
    @volatile var cacheValid: Boolean = true
    val rwl: ReentrantReadWriteLock = new ReentrantReadWriteLock();

	  def processCachedData() {
      rwl.readLock().lock()
      if (!cacheValid) {
        // Must release read lock before acquiring write lock
        rwl.readLock().unlock()
        rwl.writeLock().lock()
        // Recheck state because another thread might have acquired
        // write lock and changed state before we did.
        if (!cacheValid) {
          data = new Object()
          cacheValid = true
        }
        // Downgrade by acquiring read lock before releasing write lock
        rwl.readLock().lock()
        rwl.writeLock().unlock() // Unlock write, still hold read
      }
	    println(data.toString()) // Use data
	    rwl.readLock().unlock()
    }
	  
	  //Locks can be idiomatically acquired and released in a try-finally fashion:
	  val m = Map.empty[Int, Int]
	  val w = rwl.writeLock()
	  w.lock()
    try { m.+((0, 1)) }
    finally { w.unlock() }
    
    //Scala does not have natively concurrent collections but Java has and we can make good use of them sure:
    new ConcurrentHashMap[String, String] asScala
    
    //JAVA TRIVIA: an annotation to generate accessors - un fagiolino e una classe java idiomatica
    class POJO(@BeanProperty var mutable: String)
    val x = new POJO("LOL")
    x.getMutable
    x.setMutable("lol")
    
    }
    
    object Dynamic {
      //Scala supports dynamic resolution: we can make use of placeholders in case a property or method are not defined
      case class DynImpl(i: Int) extends Dynamic {
        def selectDynamic(name: String) = s"Method $name is not implemented"
      }
      case class Poop(j: Int)
      DynImpl(0).foo //it works!
      //Poop(4).foo pooooor pooop :(
    }
    
    object Continuation {
  
      import util.continuations._
  
      //A continuation is a function representing what to do as the following step of a computation. It can be seen as
      //the functional counterpart of the imperative goto, a continuation moving across states rather than pure code
      //lines. In a continuation-passing-style programming, every function accepts (at least) a continuation parameter 
      //too along with its regular ones, representing what it will be called with its computation result. In CPS no 
      //function ever returns to its caller, when it is done it is instructed what to call, thus giving a chain of
      //computations. Possibly more than one continuation parameter exist, for branching. 
    
      //This is some code which reads bytes in a blocking way. We read a byte and we return it, simple and plain. This
      //code is not ok for say a web framework like Play in that whenever the user asks for those bytes the flow is
      //synchronous and he/she is blocked :[
      def readFirstCiaoByte: Byte = "ciao".getBytes().apply(0)
      val byte1 = readFirstCiaoByte
      println("first byte = " + byte1)
      val byte2 = readFirstCiaoByte
      println("again = " + byte2)
      
      //Here instead we go, provided some machinery, asynchronously. 'myCallback' is a registered framework
      //variable to be called whenever the framework IO reads a byte with readFirstCiaoByte, whose computation we
      //assume to be slow. The idea is to compose the two sequential long byte reads but immediately returning for 
      //both of them.
      var myCallback: Byte => Unit = null
      def read(callback: Byte => Unit): Unit = {
        myCallback = callback  
      }
      read { byte1 =>
        println("yes again = " + byte1)
        read { byte2 =>
          println("and again = " + byte2)
        }
      }
      //The same code with Scala continuations. 'reset' delimits the continuation and 'shift' is a closure whose exec
      //triggers the code after it down to the end of the reset block (the shift closure is actually what is formally
      //referred to as 'continuation'). The use of a continuation here is even more clearly that of a pause-resume thing.
      //The shift's call the read function with what follows as a callback; both the read and shift functions are 
      //returning right away. The "read bytes flow" is paused with the shift, and resumed when the callback they register
      //is called, which is actually as soon as a byte is read.
      val x = reset {
        val byte1 = shift(read)
        println("byte1 = " + byte1)
        val byte2 = shift(read)
        println("byte2 = " + byte2)
      }
        
      //The famous exoteric example: the callback is '+ 1', which is a valid Int => Int function, therefore the result of
      //the shift block is (((7+1)+1)+1)
//      reset {
//        shift{ k: (Int => Int) => k(k(k(7))) } 
//        + 1
//      } * 2
      //This is what's actually happening: we apply a function accepting an integer function and composing itself on the
      //value 7 to the function x + 1. It like the shift block is replaced by 'x', and this plus whatever is passed as 'k'
      ((k: Int => Int) => k(k(k(7))))((x: Int) => x + 1)
      
      //Even worse. Result is 70 - (((7*2)+1)*2)*2
      def baz() = {
        reset((1 + shift((k: Int => Int) => k(k(k(7))))) * 2) 
      }
      baz()
    }
  }
}