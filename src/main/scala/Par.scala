import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A]{
      def call = a(es).get
    })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  /* This version respects timeouts. See `Map2Future` below. */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar[A : Ordering](parList: Par[List[A]]): Par[List[A]] =
     map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]())){(c, acc) =>
      map2(c, acc)(_ :: _)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[Option[A]]] =
      as map (x => asyncF((a: A) => if (f(a)) Some(a) else None)(x))

    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(par: Par[A], par2: Par[A]): Boolean =
    par(e).get == par2(e).get

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(b => if(b) t else f)

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key)(choices)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    es => run(es)(f(run(es)(a).get) )

  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)



  /*
  Note: this implementation will not prevent repeated evaluation if multiple
  threads call `get` in parallel. We could prevent this using synchronization,
  but it isn't needed for our purposes here (also, repeated evaluation of pure
  values won't affect results).
  */
  case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                               f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))
    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis; val at = stop-start
        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }

  }
}
