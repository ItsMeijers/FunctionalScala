import Prop._
import RNG._
import Stream._
import java.util.concurrent._
import Par._

case class Prop(run: (MaxSize, TestCases,RNG) => Result) {

  def check(p : => Boolean): Prop = Prop{ (_, _, _) =>
    if(p) Proved else Falsified("()", 0)
  }

  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => {
      this.run(m, n, rng) match {
        case Passed => p.run(m, n, rng)
        case x => x
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => {
      this.run(m, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(m, n, rng)
        case x => x
      }
    }
  }

  def tag(msg: String) = Prop {
    (m, n,rng) => run(m, n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
      }
  }

}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ Ok, proved property")
    }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case object Proved extends Result {
  def isFalsified = false
}

case class Falsified(
  failure: FailedCase,
  successes: SuccessCount) extends Result {

  def isFalsified = true

}


case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(this.listOfN)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](s2: Gen[B]): Gen[(A, B)] =
    for {
      a <- this
      b <- s2
    } yield (a, b)

}

object **{
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(gen.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(s => listOfN(s, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(s => g.listOfN(s max 1))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample)
    )
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))

  def unfold[A, B](start: B)(f: B => Option[(A,B)]): Stream[A] = f(start) match {
    case Some((elem, next)) => elem #:: unfold(next)(f)
    case None => empty
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace().mkString("\n")}"


  // This generator creates a fixed thread pool executor of 75% of the time
  // and an unbounded on 25% of the time
  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))

}

object TestCases {
  import Gen._
  val smallInt = choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val headSort = forAll(listOf1(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.head == ns.min
  }

  val lastSort = forAll(listOf1(smallInt)) { ns =>
    val sorted = ns.sorted
    sorted.last == ns.max
  }

  val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    (ns.isEmpty || nss.tail.isEmpty || !ns.zip(ns.tail).exists {
    case (a,b) => a > b
    }) && !ns.exists(!nss.contains(_)) && !nss.exists(!ns.contains(_))
  }

}
