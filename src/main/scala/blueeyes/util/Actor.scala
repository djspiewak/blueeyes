package blueeyes.util

import java.util.concurrent.{Executors, ConcurrentHashMap, ConcurrentMap, BlockingQueue, LinkedBlockingQueue}

trait Strategy {
  def submit[A, B](f: A => B, work: (A, Future[B])): Unit
}

trait StrategySequential {
  implicit val strategy = new Strategy {
    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      val (request, response) = work

      try {
        response.deliver(f(request))
      }
      catch {
        case e => response.cancel(e)
      }
    }
  }
}

trait StrategyThreaded1 {
  private val sequential = new StrategySequential { }
  private val executor = Executors.newSingleThreadExecutor

  implicit val strategy = new Strategy {
    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {
      executor.execute(new Runnable {
        def run = sequential.strategy.submit(f, work)
      })
    }
  }
}

trait StrategyThreadedN {
  import java.util.concurrent.ExecutorService

  def executorService: ExecutorService

  implicit val strategy = new Strategy {
    import java.util.concurrent.locks.{ReentrantReadWriteLock => RWLock}

    val assignedQueues    = new ConcurrentHashMap[_ => _, BlockingQueue[(_, Future[_])]]
    val unassignedQueues  = new LinkedBlockingQueue[(_ => _, BlockingQueue[(_, Future[_])])]
    val transferLock      = new RWLock()

    def submit[A, B](f: A => B, work: (A, Future[B])): Unit = {

    }
  }
}

sealed trait Actor[A, B] extends PartialFunction[A, Future[B]] { self =>
  def map[BB](f: B => BB): Actor[A, BB] = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).map(f)
  }

  /** Actor composition.
   *
   * {{{
   * val aToC = aToB >>> bToC
   * }}}
   */
  def >>> [BB](that: Actor[B, BB]): Actor[A, BB] = new Actor[A, BB] {
    def isDefinedAt(a: A): Boolean = self.isDefinedAt(a)

    def apply(a: A): Future[BB] = self.apply(a).flatMap(that)
  }
}

object Actor {
  /**
   *
   * {{{
   * val actor = Actor(MyActorState(x, y, z)) { state =>
   *   case MyMessage1(x, y, z) => x + y * z
   *   case MyMessage2(x)       => x
   * }
   * }}}
   */
  def apply[A, B, S](state: => S)(implicit strategy: Strategy) = (factory: S => PartialFunction[A, B]) => {
    val createdState = state

    apply[A, B](factory(createdState))
  }

  def apply[A, B](f: PartialFunction[A, B])(implicit strategy: Strategy): Actor[A, B] = apply(f)

  def apply[A, B](f: PartialFunction[A, B], onError: Throwable => Unit)(implicit strategy: Strategy): Actor[A, B] = new Actor[A, B] { self =>
    def isDefinedAt(request: A): Boolean = {
      try f.isDefinedAt(request)
      catch {
        case e1 =>
          try onError(e1)
          catch {
            case e2 => e2.printStackTrace
          }

          false
      }
    }

    def apply(request: A): Future[B] = {
      val response = new Future[B]

      if (!isDefinedAt(request)) {
        response.cancel(new Exception("This actor does not handle the message " + request))
      }
      else {
        strategy.submit(f, (request, response))
      }

      response
    }
  }
}