package all.about.scala

import util.{Try, Success, Failure}
import concurrent._
import ExecutionContext.Implicits.global
import akka.actor._
import akka.actor.SupervisorStrategy._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import akka.testkit.{TestKit, TestProbe}
import concurrent.duration._
import rx.lang.scala._
import rx.lang.scala.Notification.{OnCompleted, OnError, OnNext}
import rx.lang.scala.subjects.{AsyncSubject, BehaviorSubject, ReplaySubject}
import rx.subscriptions._
import akka.actor.ActorSelection.toScala
import akka.util.Timeout.durationToTimeout
import java.util.concurrent.atomic.AtomicReference

object QuizCode {

  object Monads {
    
    //In functional programming any data type supporting two operations, unit and bind, obeying three laws (see below)
    //can be referred to as a monad:
    
    trait Monad[T] {
      def unit(t: T): Monad[T] = ???
      def bind(f: T => Monad[T]): Monad[T] = ???
    }
    
    //In pratice, their counterparts apply and flatMap in Scala allow for more flexibility, eg flatMap can transform
    //to another type. List and Option are monads:
    
    val xs = List(1) //apply is the unit
    val xs_fm = xs.flatMap(_.toString) //flatMap is the bind
    
    val s = Some(1) //applying an Option[T] means creating a Some[T]
    val s_fm = s.flatMap(Some(_))
    
    //Map is a secondary operation we can define in terms of unit and bind, with two equivalent definitions:
    
    trait MonadWithMap[T] extends Monad[T] {
      def map(f: T => T): Monad[T] = bind(x => unit(f(x)))
      def composeMap(f: T => T) = bind(f andThen unit)
    }
    
    //The laws are:
    class Foo(i: Int) extends Monad[Int]; val foo = new Foo(1)
    val f: Int => Foo = (x: Int) => new Foo(x + 1); val g: Int => Foo = (x: Int) => new Foo(x * 2);
    
    //associativity:
    assert((foo bind f bind g) == (foo bind (x => f(x) bind g)))
    
    //left unity:
    assert((foo unit(1) bind f) == f(1))
    
    //right unity:
    assert((foo.bind(x => foo.unit(x)) == foo))
        
    //Another important pseudo-monad is Try, and thanks to the apply, map and flatMap operations it can be threaded 
    //on with for cycles:
    
    def computeX = Try {3 / 0}    
    def computeY = Try{0 / 3}
    def h(i: Int, j: Int) = 42
    
    for {x <- computeX; y <- computeY} yield h(x, y)
    
    //h(x, y) will be executed iff both functions return with Success, otherwise Failure(ex) will be returned, where
    //ex is either of the exceptions
    
    //Pseudo-monad because left unit fails:
    
    def w(i: Int) = Try(i+42)
    assert((Try(3/0) flatMap w) != w(3/0))
    
    //basically mapping on an exception is prevented, which makes sense
    
    //A functor is instead defined with a unit and a flatMap (flatten + map) method (chapter 11 of SiD might be a 
    //good starting point for a full scalazeee investigation). An applicative accepts a functor (eg List) containing
    //functions, ie F[A=>B], an implicit evidence of type F[A] and returns F[B]
  }
  
  object Trai {
    
    
    //Try is a pseudo monad. We use a Try when we want to return a single computation result possibly yielding an
    //exception:
    Try(1/0)
    
    //An Iterable is a trait whose contract is a function returning an Iterator, an object which allows us to go thru
    //the elements of the collection implementing Iterable, one by one. It is used when the computation yields multiple
    //results and it is navigable as we said.
    List(1, 2, 3).iterator.next
    
    //Iterable's are not a generalization of Try's, for a number of different reasons, but whenever we want to handle
    //multiple results which can fail we can of course do something like:
    List(1, 2, 3).iterator.map(i => Try(i / 0))
    
    //Iterable's are monads:
    List(1).flatMap(_.toString)
    
    //Both Try and Iterable represent a synchronous computation The code argument to the Try is executed normally, ie
    //in the same thread, and this is not ideal if the code in question is blocking. An Iterator is a resource to
    //access a collection of values, and these values are already defined in order to be navigated (they could be
    //Future's anyway and this is ok, but what we mean is that the collection has to be there).
    
    //A Future can be seen as a Try executed asynchronously. Still a single and possibly-failing computation, but in 
    //a different thread. The advantage of this when executing asynchronous code is the possibility to continue running
    //the current thread, which can be notified when the spinoff ends, successfully or with a failure.
    Future(println("It takes a long cpu time to interrupt for an output call"))
    //We needed to import the thread pool executor basically, that component which orchestrates thread load and
    //execution.
    
    //Try and Future are dual in the sense of category theory; without going into details, the reason is that Future
    //onSuccess: (Try[T] ⇒ Unit) ⇒ Unit
    //Reverting this we get Unit ⇒ (Unit ⇒ Try[T]) which is equal to () ⇒ (() ⇒ Try[T]), ie Try[T].
    
    //Finally, in the observer pattern, an entity, the observer, subscribes to another, the observable, which notifies
    //it whenever its own state changes, usually invoking methods the observer registered. An Observable can be seen
    //as a generalization of a Future, in the sense that it collects a number of asynchronous computations and allows
    //for callbacks to be registered on it.
    Observable.interval(1 second)
    //This observable is the collection {0, ..., max_long}, but it's not all there yet, elements are emitted one 
    //after the other each second. Of course, being a monad, maps and company are still there, and we can apply
    //functions to be realized when the value is there (it is a collection of futures after all!).
    
    //For a type T, the proportion is as follows:
    //T : Iterable[T] = Future[T] : Observable[T]
    //where on the left hand we are not dealing with exception handling at all (we would be partially if putting Try[T]
    //instead, and the proportion passes through duality.
    
    //So:
    //T: single value of type T, already available, to be used now
    //Iterable[T]: multiple values of type T, ready to get one by one through an Iterator
    //Future[T]: single value of type T, which may or may not be available already, and if it is may or may not have
    // succeeded
    //Observable[T]: multiple values of type T emitted as they come; each of these may yield a failure
    
    //Back to Try, main methods:
    assert(Try(1).flatMap(_ => Try(toString)) == Try("1")) // binding (see above)
    assert(Try(Try(1)).flatten == Try(1)) // flattening
    assert(Try(1).map(Option(_)) == Try(Some(1))) // mapping aka binding for dummies
    assert(
      Try(1).filter(_ % 2 == 0) == Failure(throw new java.util.NoSuchElementException("Predicate does not hold for 1"))
    ) // filtering on a Try returns a Success of the value if the predicate is satisfied, a Failure with an nsee
      // otherwise; notice that it would be possible to call filter on a Failure only if Nothing was inhabited and 
      // with methods, because Failure <: Try[Nothing]
    assert(Try(1/0).recoverWith{case ae: java.lang.ArithmeticException => Try(ae.getMessage)} == Try("/ by zero"))
    // when the Try yields an exception, we fall back with the given partial function
    
    //A Future is a monad too, offering the capabilities to handle with failures and latency. By using a Future, we
    //can notify consumers when a computation is done: less ceremoniously, we can register different callbacks for
    //different events:
    Future(1).onComplete{
      case Success(s) => println(s)
      case Failure(ex) => println(ex.getMessage)
    }
    Future(1).flatMap{_ => Future(toString)}
    
    //A Promise is a container which can be realized with a value or an exception
    val p = Promise[Int]()
    //a little blank we fill with some result or failure:
    p.success(1)
    p.failure(throw new Exception("Doing this will determine an ise because the Promise was already fulfilled"))
    //but just once
  }
  
  object Observabol {
    
    
    
    val x = Observable.interval(1 second)
    
    //When subscribing to an Observable stream usually we want to register the onNext and onFailure callbacks:
    val y = x.subscribe((i : Long) => println(i), (t: Throwable) => System.err.println(t.getMessage))
    //An Observable is a trait, and its extendor can be a hot or cold observable.
    //A hot observable is one where the source is shared by all the subscribers and its computation is independent of
    //them, UI events for example. A cold observable is one where computation resources are allocated for each
    //subscriber independently, like in some http keep-live feed. 
  
    //Unsubscribing: we don't want to receive notifications anymore (the callbacks will stop to be called)
    y.unsubscribe()
    //Hot observables continue with their computation regardlessly of registered subscribers, whereas a cold observable
    //might want to suspend, or halt, the computation in case no one is listening, or also go on, this is the case for
    //the replay observable, an observable which notifies a new subscriber of past events too (I subscribe at tick 4,
    //I get info for ticks 0, ..., 3 too).
    //The unsubscribe method is of course idempotent.
    
    //Subscription's:
    
    //BooleanSubscription, a Subscription that can be checked for unsubscription in an easy way:
    val bs = new BooleanSubscription()
    
    //CompositeSubscription, multiple subscriptions packed together
    val cs = new CompositeSubscription()
    //Unsubscribing a composite will cause all of the contained subscriptions to be unsubscribed as well automatically.
    //Adding a new Subscription has no effect if the container was not unsubscribed yet. If we add a subscribed
    //to an unsubscribed the former will be flagged unsubscribed too, but the viceversa does not (seem to) hold.
    
    //A MultipleAssignmentSubscription is a container for a single subscription that can be replaced along the
    //computation; this proxy cannot be empty
    val mas = new MultipleAssignmentSubscription()
    
    
    //Creating Observable's, base cases:
    
    //An Observable that never emits
    Observable.never
    
    //One that emits an error
    Observable.error(new Exception())
    
    //These are internally built with the create method (see below for an example with create)
    
    //Real observables can be created from existing Iterable's or as emitters:
    val f = Observable.from(List(1, 2, 3))
    val i = Observable.interval(1 second)
    //or from Future's:
    val ff: Observable[Int] = Observable.from(Future(1))
    
    //startWith: first we emit this' ticks, then that's 
    f ++ (Observable.from(List(4, 5, 6)))
    //filter:
    f filter (_ %2 == 0)
    //map:
    f map (_ + 1)
    //reduce:
    f reduce ((_, _) => 2)
    
    //A Subject is at the same time an Observable and Observer, and therefore it can be seen as a channel. Four types:
    //Subject: normal emitter
    //ReplaySubject: caches its ticks and replays the whole buffer whenever there's a new subscriber
    //BehaviorSubject: caches and replays only the last one
    //AsyncSubject: a BehaviorSubject that outputs only after completion
    val subjects = List(Subject[Int](), ReplaySubject[Int](), BehaviorSubject[Int](55), AsyncSubject[Int]())
    val s0 = subjects(2).subscribe(println(_)) //will print 55, the default value
    subjects.map(_.onNext(1))
    subjects.map(_.onNext(2))
    subjects.map(_.onNext(3))
    val s1 = subjects(0).subscribe(println(_)) //nothing to print
    val s2 = subjects(1).subscribe(println(_)) //will print 1, 2, 3
    val s3 = subjects(2).subscribe(println(_)) //will print 3
    val s4 = subjects(3).subscribe(println(_)) //nothing to print yet!
    subjects(3).onCompleted()
    val s5 = subjects(3).subscribe(println(_)) //now that's a 3
    //There is a possible analogy between Promise and Subject in being both containers where we can put values into
    //on one side and get them on the other, and both convertable to Future and Observable.
    
    //The result of a Future is a Try, and we can access the Success/Failure in the onComplete method. An Observable
    //can be materialized in the type Notification instead:
    f.materialize.map{
      case OnNext(v) => Success(v)
      case OnError(err) => Failure(err)
      case OnCompleted(_) => Success()
    }
    
    //The suggested way to deal with Future's is with nonblocking combinators:
    Future(1).map(_ + 1)
    //but of course you can also block:
    Await.result(Future(1), 10 seconds)
    
    //Analogously, you can obtain a blocking version of an Observable:
    val fBlock = f.toBlockingObservable
    //so you can, for example, materialize it into an Iterable:
    fBlock.toList
    //Some operators:
    //this foreach execution blocks until the observable is completed
    fBlock.foreach(println(_)) 
    //this returns an Iterable whose Iterator blocks on next and hasNext until the underlying Observable ticks again
    fBlock.next  
    
    //A Scheduler is the Observable counterpart of ExecutionContext's for Future. A Scheduler can be used for
    //scheduling tasks of course, but also to allow such things as unsubscribing from an infinite stream, and it's 
    //generally used in different Observable methods. There're different types of schedulers under:
    import rx.lang.scala.schedulers._
    
    //RX TRIVIA BREAK
    val xs = Observable.create{observer: Observer[Int] =>
      observer.onNext(42) 
      observer.onCompleted() 
      observer.onNext(4711) 
      Subscription()
    }
    xs.subscribe(println(_))
    //this will print only 42 clearly
  }
  
  object Attori {
    //The easiest way in Java to assure a method is not executed concurrently by multiple threads is the synchronized
    //keyword; in Scala this is a block:
    class Account {
      private var fund = 1000
      def withdraw(amount: Int) = synchronized{fund - amount}
    }
    //This is inherited from Java and we can synchronize on Any-thing. When we enter the synchronization we exclusively
    //own the resource.
    //	Programming concurrent applications in this fashion is prone to synchronization error, deadlocks, and the
    //overhead of locks acquisition must be taken into account too. Other than this, the coupling is strong too.
    
    //The actor model to the rescue. An actor is a computational entity with a known identity with a defined behavior,
    //interacting with other actors through asynchronous message passing:
    abstract class Attore extends Actor
    //When receiving a message all an actor can do is:
    //-sending a message
    //-creating an actor
    //-changing its behavior
    //-carrying out some internal computation
    //-processing a message, one at a time
    //The behavior cannot be accessed directly, only executed with respect to a message! This is the real difference
    //from what we have above.
    
    //The behavior of an actor is defined by its receive method and context information, such as the sender of the
    //current message and the possiblity to stack or replace the current behavior through become(), comes from ActorContext
    //that is a value from the Actor class.
    //There is no concept such as "global synchronization", or clock, the actors are independent and concurrent. 
    //Behind the scenes, an actor is a thread. It receives and picks messages sequentially, and the computation is
    //executed before picking a new one; if changing the behavior is part of this, then the new behavior is adopted
    //before the new message; message processing is, from the actor lifecycle point of view, an atomic action. This
    //all means that the execution associated with a message is equivalent to a synchronized one, where no other
    //external client can be concurrently served because their message will be opened at a later time.
    
    //The state of an actor can be modeled both implicitly (imperative) or explicitly (functional):
    class Attore1 extends Actor {
      private var i = 0
      def receive = {
        case i: Int => this.i = i
      }
    }
    class Attore2 extends Actor {
      private def counter(i: Int): Receive = {
        case i: Int => context.become(counter(i))
      }
      def receive = counter(0)
    }
    
    //An Actor cannot be instantiated with a new, the right way is:
    ActorSystem().actorOf(Props[Attore])
    //where we create an actor system and request an instance by passing a particular configuration properties instance.
    
    //We can also choose the thread pool it will join; this is useful for separating the execution contexts of clients
    //and services from the one of work-intensive actors:
    ActorSystem().actorOf(Props[Attore].withDispatcher("isolated"))
    //This dispatcher needs to be configured, for example:
    //isolated.fork-join-executor {
    //  parallelism-min = 4
    //  parallelism-max = 4
    //}
    
    //When creating and stopping child actors instead we start from the context:
    class AttorePapà extends Attore {
      override def receive = {
        case ("kill", ref: ActorRef) => context.stop(ref)
        case "children_broadcast" => context.children.foreach {_ ! "BCAST"}
        case _ => sender ! context.actorOf(Props[Attore])
      }
    }
    
    //Best practice: messages are case objects/classes defined in the companion of the actor:
    object AttorePapà {
      case object SAROPAPÀ
      case class LaMammaE(mama: String)
    }
    
    //Communication is defined to be unreliable, guarantees can be {at most, at least, exactly}-once, and ordering 
    //may or may not be maintained. Akka implements an at most once, which fits particularly remotely distributed
    //systems, that Akka supports, and message ordering.
    
    //!-tell fires a message and returns unit; ?-ask returns a Future instead, representing the reply, if any. Ask is
    //more demanding than tell as a call, but it can come in handy when communicating with an actor from a class that
    //it is not. With ask, an internal actor is created which sends a message to the target actor and bridges the reply
    //onto a future.
    //This communication pattern can be combined with the pipeTo pattern to forward the Future value to another actor.
    abstract class AttoreMammà(papà: ActorRef, nonna: ActorRef)(implicit timeout: Timeout) extends Attore {
      override def preStart {
        papà ? "ciao" pipeTo nonna
      }
    }
    
    //An example of async computation with the ask pattern:
    class Speedy(a: ActorRef, b: ActorRef, c: ActorRef) extends Actor {
      private implicit val timeout: Timeout = 2 seconds
      def receive(): Receive = {
        case "compute" => 
          val x = (a ? "x").mapTo[String]
          val y = (b ? "y").mapTo[String]
          val z = (c ? "z").mapTo[String]
          for (resX <- x; resY <- y; resZ <- z) yield(context.parent ! resX+resY+resZ)
      }
    }
    
    //Actors can proxy messages:
    class Proxy extends Actor {
      def receive(): Receive = {
        case m@"message" => context.children.foreach(_.forward(m))
      }
    }
    //Two types of proxying:
    //-stateful, for example:
    //  simple round robin: request 1 is forwarded to worker a, 2 to worker b, 3 c etc, ..., n to a, n+1 to b...
    //  smallest queue: if some kind of requests backlog is maintained for each worker, we route to the least loaded
    //  shared work queue: jobs are enqueued by the router and pulled by the routees
    //  adaptive: any technique dispatching work on the basis of the state of the workers
    //-stateless:
    //  random
    //  consistent hashing: the request is hashed and the hash space mapped onto the workers (hash 00 goes to a, 01
    //   to b etc); a hashing algorithm is consistent if whenever the hash table, the number of workers in our case,
    //   is resized, only K/n keys need remapping, where K is the number of keys (requests) and n the initial number
    //   of buckets/workers; we need consistency since nodes in a distributed system come and go and we want to spend
    //   a reasonable time in rewriting the routes
    
    //Logging: provided by Akka through dedicated actors relying on log4j
    abstract class Filosofo extends Actor {
      override def postStop {
        context.system.log.info("Finito!")
      }
    }
    
    //An actor receives messages periodically, and if the period is too long then we can do something:
    class Impaziente extends Actor {
      context.setReceiveTimeout(10.seconds)
      def receive: Receive = {
        case ReceiveTimeout => println("printlining is not the proper way to ask Where you fellow actors at?")
      }
    }
    //by which if no message is received for 10 s a particular message is sent by the system to the actor; the timer
    //is reset at each reception
    
    class LoSchedulatore extends Actor {
      private implicit val timeout: Timeout = 5.seconds
      //scheduling a block of code to be executed in 10 seconds (methods for repeating exist too):
      private val scheduler = context.system.scheduler
      private var cantTouchThis = "MCHammer"
      scheduler.scheduleOnce(10.seconds) {
        self ! "automessage"
      }
      //scheduling a message to be sent to an actor
      scheduler.scheduleOnce(10.seconds, self, "automessage")
      //scheduling a Runner's run() method to be called
      scheduler.scheduleOnce(10.seconds, new Runnable(){def run() = self ? "automessage"})
      //It is very dangerous to "close over actor states", ie referring to possibly mutable state when scheduling; the
      //code is likely to be executed in a different thread and race conditions may rise; accessing the actor state
      //from the outside breaks the encapsulation guaranteed by the fact that the computation is orchestrated through
      //messages:
      def receive: Receive = {
        case _ => cantTouchThis = "CantTouchThis"
      }
      val dontDoThis = scheduler.schedule(0.seconds, 5.seconds){
        if (cantTouchThis == "MCHammer") scheduler.scheduleOnce(2.seconds)(exit(0))
      }
      
      //Testing an actor means asserting on its observable state, its messages:
      val howdy = "How are you?"
      val eppi = "happy"
      val sadde = "sad"
      class Toggle extends Actor {
        def happy: Receive = {
          case `howdy` => sender ! eppi; context become(sad)
        }
        def sad: Receive = {
          case `howdy` => sender ! sadde; context become(happy)
        }
        def receive = happy
      }
      implicit val system = ActorSystem("Testsys")
      val toggle = system.actorOf(Props[Toggle])
      val p = TestProbe()
      p.send(toggle, howdy)
      p.expectMsg(eppi)
      p.send(toggle, howdy)
      p.expectMsg(sadde)
      p.send(toggle, "boh")
      p.expectNoMsg()
      
      //And more idiomatically:
      new TestKit(ActorSystem("TestSys"))  {
        val toggle = system.actorOf(Props[Toggle])
        toggle ! howdy
        //...
      } 
    } 
  }
  
  object ActorFail {
    //In an actors-backed application failures should be reified into messages and properly delivered for handling.
    //An actor system is a tree; the root actor '/' fathers other two guardian actors '/user' and '/system', rooting
    //user defined (actorOf's) and system (es logging) actors respectively. The root actor is actually son of a
    //synthetic actor, the "bubble walker".
    
    //Regular Akka supervision is parent-based: the parent can define a supervision strategy for its children actors.
    //The default strategy, SupervisorStrategy.defaultStrategy is a OneForOneStrategy, action is taken on the actual 
    //failing actor only, and in particular the actor is stopped if initialization fails or it is killed and restarted 
    //in case of exception. An AllForOne strategy is instead needed when something needs to be done on the siblings 
    //as well, because the computation carried on by a single actor is tightly coupled with other actors. The 
    //exception objects are sent as messages:
    abstract class ExampleActor extends Actor {
      override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 10, withinTimeRange = 1 minute) {
		    case _: ArithmeticException      ⇒ Resume
		    case _: NullPointerException     ⇒ Restart
		    case _: IllegalArgumentException ⇒ Stop
		    case _: Exception                ⇒ Escalate
      }
    }
    //When restarting an actor its ActorRef is maintained, ie it can still be referred with that ref, but its internal
    //state (receive stack, vars) is lost. Restarts are not visible externally. Children actors are recursively 
    //restarted. Good design practice: keep relevant state close to the root of the system, push risk down the leaves.
    //-dangerous ie likely to fail with throw task should be carried out in a subordinate actor (eg the executor
    // creates a runner child)
    //-childrens are monitored and supervised (see below)
    //-childrens report success to their parents; failure can be reported via exceptions (so that we can use the
    // monitoring facilities in the parent)
    //-when done, the children shuts down
    
    //Lifecycle hooks:
    //-preStart
    //-preRestart
    //-postRestart
    //-postStop
    
    //Lifecycle monitoring/deathwatch: an actor is interested in the lifecycle of another. Since restarts are not
    //visible, the only transition is from alive to dead.
    //Supervision: reaction to failure
    //Monitoring: reaction to termination
    
    class ExamplarActor(ref: ActorRef) extends Actor {
      override def preStart() {
        context.watch(ref)
      }
      def receive: Receive = {
        case "unwatch" => context.unwatch(ref)
        case Terminated(_) => context.system.log.info("Actor " + ref + " is dedda") 
      }
    }
    
    //In the original actor model, there is no such thing as broadcast channels, actors can send messages to other
    //actors only. Akka EventStream allows for such channels. An EventStream has an arbitrary number of topics and
    //actors can subscribe to a subset of them.
    
    class Producer extends Actor {
      def receive: Receive = {
        case s: String => context.system.eventStream.publish(s)
      }
    }
    
    class Consumer extends Actor {
      override def preStart() {
        context.system.eventStream.subscribe(self, classOf[String])
      }
      def receive: Receive = {
        case s: String => context.system.log.info("Received radio message: " + s)
      }
    }
    
    //Unhandled messages, ie messages that don't fall into any receive bucket, are processed with an unhandled method,
    //user overridable, which by default throws an exception for DeathWatch Terminated messages and publishes the
    //message to the EventStream otherwise.
    
    //We said that actor state is not saved after a restart, but we might be interesting in this or even persisting
    //the state. We can decide to save either the current state, periodically, or just changes when we have them. A
    //very good approach is a mixed one where we save deltas but schedule a snapshot too.
    
    //In general we want to store the messages we send out together with their acks when we get them. Restoring the
    //state of the computation for a system failure then means replaying the messages all around, whereas in the case
    //of a single-actor failure we might want to resend only those for which we couldn't get an ack.
    
    //Event-sourcing: 
    //-generate change requests, events, instead of modifying local state
    //-persist and apply these changes
    
    //The proper way to go is to buffer new messages while persisting received ones, in order to guarantee both
    //correct persistence and behavior in a lot of situations (eg a server can accept a max no of transactions a day
    //from its clients, while i am persisting one i cannot accept and process a new one because if a failure happens
    //during the persistence then when recovering we might lose both).
    
    //Akka allows for an actor to buffer messages for later processing:
    
    class Buffer extends Actor with Stash {
      def receive: Receive = {
        case "i'll take a break from my calculations now" => context.become(stashin)
        case "message" => 2+5+7
      }
      private def stashin(): Receive = {
        case "mobbasta" => context.unbecome(); unstashAll()
        case _ => stash()
      }
    }
  
	  //Akka actor systems can be remotely distributed in a way transparent to the user
	  val system = ActorSystem("y0")
	  val ref = system.actorOf(Props[Buffer], "buffer")
	  assert(ref.path == "akka://y0/user/buffer") //purely local! reachable from the outside via eg:
	  //akka.tcp://y0@1.2.3.4:2552/user/buffer
 
	  //Systems are small netty httpd binding to the available addresses at port by default 2552.
	  //Paths of non-existing actors can be used, if they are not there then the message will go to DeadLetters. An
	  //ActorRef points to an actor that was started instead.
	  
	  //The proper way to get a remote actor ref from its absolute path; an actor selection is a set of one or more
	  //actors:
	  class Remoter(path: ActorPath) extends Actor {
	    override def preStart() {
	      context.actorSelection(path) ! Identify((path, sender))
	    }
	    def receive(): Receive = {
	      case ActorIdentity((path, client), Some(ref)) => context.system.log.info("Got actor "+ref)
	      case "broadcast to them" => context.actorSelection("/user/them/*") ! "tchao"
	      case _ => context.system.log.info("Actor "+path+" does not exist")
	    }
	  }
	  
	  //Akka systems can be made into nodes of a cluster thanks to a dedicate infrastructure. In the cluster all of the
	  //actors know eachother and information is spread via gossip.
	  
  }
  
  object TituliDiCoda {
    
    //Strong consistency. Say a source can be written and read concurrently by many; we have s.c. if at any time after
    //an updation is completed any query will return the fresh info:
    private var field = 0
    def update(f: Int => Int): Int = synchronized {
      field = f(field)
      field
    }
    def read(): Int = synchronized { field }
    //The synchronized pattern yields itself to this kind of consistency guarantee, and what happens here is that 
    //basically reads and writes are fully serialized. The code is blocking as we know.
    
    //Weak consistency - the fresh value is returned only provided some conditions are met:
    @volatile private var volField = 0
    def volUpdate(f: Int => Int): Future[Int] = Future {
      synchronized {
	      volField = f(volField)
	      volField
      }
    }
    def volRead(): Int = volField
    //The volatile annotation assures in a nutty nutshell that:
    //-no cached values are to be read for the variable; if the variable is shared among threads whenever it is
    // referenced it is refreshed; the compiler cannot get smart and try to refer to cached local values; this
    // resembles the semantics of the keyword in C where a variable is declared volatile when we know that it can be
    // updated by external agents ie it can change in ways the compiler cannot be aware of (eg updated in direct
    // access by some external device; this has nothing to do with concurrent programming!!)
    //-any read to a volatile field is guaranteed to see the last executed write by any other thread on it; never a
    // stale value may be read; actually, it is guaranteed that the all of the other variables too are seen by the
    // reader exactly how they were seen by the writer, the states coincide
    
    //What we do here is that we send the updation to be executed on possibly another thread, and even if volUpdate
    //completes by returning a Future if we read while this Future hasn't been completed yet then the "old" value is 
    //returned; notice that we are not closing over mutable state in the Future because the actions are placed in a
    //synchronized block; the value of volField cannot change ie be changed by other threads inbetween the moment we
    //pass it to f and we update it. So the condition for weak concurrency in this example is that isCompleted on the
    //Future returns true.
    
    //An evolved @volatile machinery is represented by AtomicReference:
    import java.util.concurrent.atomic.AtomicReference
		class Person(val name: AtomicReference[String]) {
		  def set(changedName: String) {
		    name.set(changedName)
		  }
		}
    
    //Eventually consistency: particular weak consistency where after un update some time during which no other 
    //updates are performed needs to pass before all readers return the same value. Akka clustering is eventually 
    //consistent.
    
  }
  
  object AttoriTipati {
    
    //A typed actor is an Akka actor wrapped in an interface so that it can be interacted with as one does for plain
    //old java objects but it is at the same time be able to interact with regular actors. In a more elaborate way
    //than using the ask pattern this lets us bridge from actor-unaware code to Akka: message sends are now modeled as
    //method calls.
    
    //What we need is the interface, a trait then, and an implementation, and we will be able to create such actors
    //through a dedicated Akka extension; an extension is a mechanism to add features to an actor system.
    
    val system = ActorSystem()
    val extension = TypedActor(system)
    
    trait Squarer {
      
      def squareDontCare(i: Int): Unit //fire-forget: invoking this method means !/tell-ing the internal actor

      def square(i: Int): Future[Int] //non-blocking send-request-reply: this ask-s the actor
      
      def squareNowPlease(i: Int): Option[Int] //blocking send-request-reply: this request blocks, returning Some if
                                               //ok, None if timeout and whatever exception
      
      def squareNow(i: Int): Int //blocking send-request-reply with exception: pure result or computation exceptions
                                 //are returned but a timeout exception is returned rather than a None
      
      @throws(classOf[Exception]) //declare it or you will get an UndeclaredThrowableException (requested by the
                                  //implementation, which is based on java proxies)
      def squareTry(i: Int): Int
   
    }
    
    class SquarerImpl(name: String) extends Squarer with TypedActor.PreRestart {
      
      def squareDontCare(i: Int) { i*i }
      
      def square(i: Int) = Future(i*i)
      
      def squareNowPlease(i: Int) = Some(i*i)
      
      def squareNow(i: Int) = i*i
      
      def squareTry(i: Int) = throw new Exception("tried :(")
      
      val childSquarer: Squarer = TypedActor(TypedActor.context).typedActorOf(TypedProps[SquarerImpl]())
      
      override def preRestart(reason: Throwable, message: Option[Any]) { system.log.debug("restarting") }
      
    }
    
    val squarer: Squarer = TypedActor(system).typedActorOf(TypedProps(classOf[Squarer], new SquarerImpl("foo")), "name")
    
    
  }
  
  object Takeaway {
    //All of these concepts and techniques find their best expression in a programming approach called reactive
    //programming, whose pillars are:
    //-responsiveness
    //-resiliency
    //-scalability
    //-event-driven computation
    //A reactive system is quick to "respond to stimuli":
    //-responsiveness: user experience unaffected by issues like heavy loads, system failures, incorrect usage
    //-resiliency: the system recovers quickly from failures; for example, in case of a partial failure system
    // performance is degraded yet acceptable, or for a fatal failure it does not take lots of time to bring the
    // system back up
    //-scalability: be able to scale-up, meaning that it is able to take full advantage of a parallel architecture,
    // as well as scale-out, it can be efficiently deployed on multiple nodes
    //-event driven: components react to events from the environment, from other components or internal, rather than
    // interacting through shared state; an event driven system yields itself to the actor paradigm
  }
}
