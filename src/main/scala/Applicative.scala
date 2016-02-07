trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def apply[A, B](fab: F[A => B])(f: F[A]): F[B] =
    map2(fab, f)(_(_)) // is same as (f, a) => f(a)

  def unit[A](a: => A): F[A]

  //derived combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]())){ (c, acc) =>
      map2(f(c), acc)(_ :: _)
    }

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]())){ (c, acc) =>
      map2(c, acc)(_ :: _)
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_, _))

  def filterM[A](fa: List[A])(f: A => F[Boolean]): F[List[A]] =
    fa.foldRight(unit(List[A]())){ (c, acc) =>
      map2(f(c), acc){ (b, l) =>
        if(b) c :: l
        else l
      }
    }

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}

object Applicative {
  val streamApplicative = new Applicative[Stream]{
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  val streamSequence = streamApplicative.sequence(List(
    streamApplicative.unit(1),
    streamApplicative.unit(2),
    streamApplicative.unit(3),
    streamApplicative.unit(4),
    streamApplicative.unit(5),
    streamApplicative.unit(6),
    streamApplicative.unit(7),
    streamApplicative.unit(8)
  ))
  // the list becomes the first element of the stream... (tranposes the list)
}
