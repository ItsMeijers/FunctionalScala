trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B =
    foldLeft(as)(m.zero)((acc, c) => m.op(acc, f(c)))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object Foldable {
  val foldableList: Foldable[List] = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

  val foldableOption: Foldable[Option] = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }
}
