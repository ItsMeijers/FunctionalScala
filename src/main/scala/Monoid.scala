import Gen._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(s: String, s2: String) = s + s2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l: List[A], l2: List[A]) = l ++ l2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, i2: Int) = i + i2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(i: Int, i2: Int) = i * i2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b: Boolean, b2: Boolean) = b || b2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(b: Boolean, b2: Boolean) = b && b2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(opt: Option[A], opt2: Option[A]) = opt orElse opt2
    val zero = None
  }

  // Dual of monoid by flipping the op (When you can compose monoids in different
  // order both cases should satisfy the monoid laws)
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y,x)
    val zero = m.zero
  }

  def firstOptionMonoid[A] = optionMonoid[A]
  def secondOptionMonoid[A] = dual(firstOptionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, f2: A => A) = f andThen f2
    val zero = (a: A) => a
  }

  def firstEndoMonoid[A] = endoMonoid
  def secondEndoMonoid[A] = dual(endoMonoid)

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen) { a =>
    m.op(a, m.zero) == a
    m.op(m.zero, a) == a
  } && forAll( for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x,y,z) ) { case (x, y, z) =>
    m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, c) => m.op(acc, f(c)))

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(x: (A, B), y: (A, B)) = (A.op(x._1, y._1), B.op(x._2, y._2))
    val zero = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero){ (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
    val zero: A => B = _ => B.zero
  }

  // TODO
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ???
}
