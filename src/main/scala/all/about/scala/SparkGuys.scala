package all.about.scala

//hints by the spark maintainers

import concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object SparkGuys {

  //don't ever use return inside a closure!
  def receive(rpc: Any): Any = {
    Future(1).onComplete { num =>
      if (num.get%2==0) return None // Do not do that!
      else { Some(1) }
    }
    2
  }
  //this will throw a NonLocalReturn exception catchable inside the receive method, it will not use None as the result
  //value for the if in onComplete
  
  
  //Concurrency. The claim is that Java concurrency stuff is safer than Scala's. Different approaches to critical area
  //access are presented. They are not meant to be mixed!!!
  
  //To use when a high degree of contention is expected (eg many threads will try to access the structure at the same
  //time) and when the number of state is finite (so that it can be encoded in the map at instance time)
  private[this] val map1 = new java.util.concurrent.ConcurrentHashMap[String, String]
  
  //To use when low contention is expected; in this case the just-in-time bytecode compiler will help removing
  //unnecessary syncs
  private[this] val map2 = java.util.Collections.synchronizedMap(new java.util.HashMap[String, String])
  
  //Manual synchronization: it is explicit yet troublesome. JIT-optimizable.
  class Manager {
    private[this] var count = 0 //the damn counter
    private[this] val map = new java.util.HashMap[String, String] //a normal hashmap
    def update(key: String, value: String): Unit = synchronized { //this method is in synchronized access
      map.put(key, value)
      count += 1
    }
    def getCount: Int = synchronized { count } //this one too - reads must be synchronized too!
  }

  //Watch out when using methods returning Iterable's and stuff on such synchronized collections - they should be
  //synchronized too of course
  def values: java.util.Collection[String] = map2.synchronized { map2.values }
   
  //These methods here are meant for map-like state. When the critical state is a primitive type, this concept is of
  //course defined in Java only, in Scala you just have types, readymade synchronization utilities are provided, the
  //atomics.
  val initialized = new java.util.concurrent.atomic.AtomicBoolean(false)
  
  //direct access, no need for sync wrap; the getAndSet method sets the value to the argument and returns the previous
  //value
  if (!initialized.getAndSet(true)) println("initialized")
  
  //The volatile annotation, with some rememberance of the meaning it has in C, basically prevents a thread from
  //caching its value, in the sense that whenever access is made the JVM makes sure that the fresh value is read. It
  //is, functionally, a subset of the atomic's in the sense that it can only safely be used if you have many readers
  //but just one writer!
  @volatile var wall = false
  
  //And finally, it is common to see internal state as private var's, but watch out that if only private different
  //instances will be able to peep out their values each other! You need to scope:
  class Foo {
    private[this] var count: Int = 0
    def inc(): Unit = synchronized { count + 1 }
  }
  //this way we are sure that the value cannot be read but in the very same instance, and of course only safely updated
  //via inc()
  
}