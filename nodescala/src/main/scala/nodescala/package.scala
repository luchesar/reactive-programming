import scala.language.postfixOps
import scala.util._
import scala.util.control.NonFatal
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }
import java.util.ArrayList
import java.util.concurrent.atomic.AtomicInteger
import java.util.NoSuchElementException

/**
 * Contains basic data types, data structures and `Future` extensions.
 */
package object nodescala {
  object Defs {
    def continueWith[S, T](fu: Future[T], cont: Future[T] => S): Future[S] = async {
      await(fu)
      cont(fu)
    }

    def allAlt[T](fs: List[Future[T]]): Future[List[T]] = async {
      var l = List[T]()
      var list = fs
      while (list.nonEmpty) {
        val f = list.head
        list = list.tail
        l = await(f) :: l
      }
      l
    }
  }
  /**
   * Adds extensions methods to the `Future` companion object.
   */
  implicit class FutureCompanionOps[T](val f: Future.type) extends AnyVal {

    /**
     * Returns a future that is always completed with `value`.
     */
    def always[T](value: T): Future[T] = {
      val promise = Promise[T]()
      promise.complete(Success(value))
      promise.future
    }

    /**
     * Returns a future that is never completed.
     *
     *  This future may be useful when testing if timeout logic works correctly.
     */
    def never[T]: Future[T] = {
      Promise[T]().future
    }

    /**
     * Given a list of futures `fs`, returns the future holding the list of values of all the futures from `fs`.
     *  The returned future is completed only once all of the futures in `fs` have been completed.
     *  The values in the list are in the same order as corresponding futures `fs`.
     *  If any of the futures `fs` fails, the resulting future also fails.
     */
    def all[T](fs: List[Future[T]]): Future[List[T]] = allPromise(fs)

    def allPromise[T](fs: List[Future[T]]): Future[List[T]] = {
      import scala.collection.JavaConversions.collectionAsScalaIterable
      val promise = Promise[List[T]]()
      val result = new ArrayList[T](fs.size)
      for (i <- 0 until fs.size) { result.add(null.asInstanceOf[T]) }
      val counter = new AtomicInteger;
      for (i <- 0 until fs.size) {
        val res = fs(i).onComplete {
          case Success(r) => {
            result.set(i, r)
            if (counter.addAndGet(1) == fs.size) {
              promise.success(collectionAsScalaIterable(result).toList)
            }
          }
          case Failure(t) => promise.failure(t)
        }
        println(res)
      }

      promise.future
    }

    def allAlt[T](fs: List[Future[T]]): Future[List[T]] = Defs.allAlt(fs)
    /**
     * Given a list of futures `fs`, returns the future holding the value of the future from `fs` that completed first.
     *  If the first completing future in `fs` fails, then the result is failed as well.
     *
     *  E.g.:
     *
     *      Future.any(List(Future { 1 }, Future { 2 }, Future { throw new Exception }))
     *
     *  may return a `Future` succeeded with `1`, `2` or failed with an `Exception`.
     */
    def any[T](fs: List[Future[T]]): Future[T] = {
      val promise = Promise[T]()
      fs.foreach(_.onComplete {
        case Success(r) => if (!promise.isCompleted) promise.success(r)
        case Failure(t) => if (!promise.isCompleted) promise.failure(t)
      })

      promise.future
    }

    /**
     * Returns a future with a unit value that is completed after time `t`.
     */
    def delay(t: Duration): Future[Unit] = Future {
      Thread.sleep(t.toMillis)
    }

    /**
     * Completes this future with user input.
     */
    def userInput(message: String): Future[String] = Future {
      readLine(message)
    }

    /**
     * Creates a cancellable context for an execution and runs it.
     */
    def run()(f: CancellationToken => Future[Unit]): Subscription = {
      val sub = CancellationTokenSource()
      f(sub.cancellationToken)
      sub
    }
  }

  /**
   * Adds extension methods to future objects.
   */
  implicit class FutureOps[T](val f: Future[T]) extends AnyVal {

    /**
     * Returns the result of this future if it is completed now.
     *  Otherwise, throws a `NoSuchElementException`.
     *
     *  Note: This method does not wait for the result.
     *  It is thus non-blocking.
     *  However, it is also non-deterministic -- it may throw or return a value
     *  depending on the current state of the `Future`.
     */
    def now: T = try {
      Await.result(f, 0 seconds)
    } catch {
      case e: TimeoutException => throw new NoSuchElementException()
    }

    /**
     * Continues the computation of this future by taking the current future
     *  and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continueWith[S](cont: Future[T] => S): Future[S] =
      Defs.continueWith(f, cont)

    /**
     * Continues the computation of this future by taking the result
     *  of the current future and mapping it into another future.
     *
     *  The function `cont` is called only after the current future completes.
     *  The resulting future contains a value returned by `cont`.
     */
    def continue[S](cont: Try[T] => S): Future[S] = {
      val result = Promise[S]()
      f.onComplete { t =>
        result.complete(Try(cont(t)))
      }
      result.future
    }

  }

  /**
   * Subscription objects are used to be able to unsubscribe
   *  from some event source.
   */
  trait Subscription {
    def unsubscribe(): Unit
  }

  object Subscription {
    /**
     * Given two subscriptions `s1` and `s2` returns a new composite subscription
     *  such that when the new composite subscription cancels both `s1` and `s2`
     *  when `unsubscribe` is called.
     */
    def apply(s1: Subscription, s2: Subscription) = new Subscription {
      def unsubscribe() {
        s1.unsubscribe()
        s2.unsubscribe()
      }
    }
  }

  /**
   * Used to check if cancellation was requested.
   */
  trait CancellationToken {
    def isCancelled: Boolean
    def nonCancelled = !isCancelled
  }

  /**
   * The `CancellationTokenSource` is a special kind of `Subscription` that
   *  returns a `cancellationToken` which is cancelled by calling `unsubscribe`.
   *
   *  After calling `unsubscribe` once, the associated `cancellationToken` will
   *  forever remain cancelled -- its `isCancelled` will return `false.
   */
  trait CancellationTokenSource extends Subscription {
    def cancellationToken: CancellationToken
  }

  /**
   * Creates cancellation token sources.
   */
  object CancellationTokenSource {
    /**
     * Creates a new `CancellationTokenSource`.
     */
    def apply(): CancellationTokenSource = new CancellationTokenSource {
      private var canceled = false

      def cancellationToken: CancellationToken = new CancellationToken {
        def isCancelled: Boolean = canceled
      }

      def unsubscribe(): Unit = {
        canceled = true
      }
    }
  }

}

