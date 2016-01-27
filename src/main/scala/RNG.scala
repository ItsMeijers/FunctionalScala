
/**
* Random Number Generations
**/

trait RNG{
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG{
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (tryInt, nextRNG) = rng.nextInt
    if(tryInt < 0) nonNegativeInt(nextRNG)
    else (tryInt, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (randomInt, nextRNG) = rng.nextInt
    (randomInt / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def boolean: Rand[Boolean] =
    map(nonNegativeLessThan(2))(i => if(i == 0) true else false)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(c: Int, list: List[Int], r: RNG): (List[Int], RNG) = {
      if(c == 0) (list, r)
      else {
        val (i, r2) = r.nextInt
        loop(c - 1, list :+ i, r2)
      }
    }
    loop(count, List.empty[Int], rng)
  }

  type State[S, +A] = S => (A, S)

  type Rand[A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r2) = s(rng)
    (f(a), r2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleMap: Rand[Double] = map(_.nextInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    fmap2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(_.nextInt, double)

  def randDoubleInt: Rand[(Double, Int)] = map(randIntDouble)(_.swap)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def sequenceLoop(r: RNG, list: List[Rand[A]], acc: List[A]): (List[A], RNG) = {
      list match {
        case Nil => (acc, r)
        case h :: t =>
          val (a, r2) = h(r)
          sequenceLoop(r2, t, acc :+ a)
      }
    }
    sequenceLoop(rng, fs, List.empty[A])
  }

  def intsSequence(n: Int): Rand[List[Int]] = sequence(List.fill(n)(_.nextInt))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = r(rng)
    f(a)(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
      rng => {
        val mod = i % n
        if(i + (n-1) + mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
      }
  }

  def fmap[A, B](f: Rand[A])(g: A => B): Rand[B] = flatMap(f)(r => unit(g(r)))

  def fmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a =>
      flatMap(rb){b =>
        unit(f(a,b))
      }
    }

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}
