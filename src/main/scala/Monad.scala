import Par.Par

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]())){(c, acc) =>
      map2(c, acc)(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]())){ (c, acc) =>
      map2(f(c), acc)(_ :: _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // Recursive version:
  def _replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List[A]())
    else map2(ma, replicateM(n - 1, ma))(_ :: _)

  // Using a higher order function
  def filterM[A](ms: List[A])(f : A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit(List[A]())){ (c, acc) =>
      flatMap(acc){ l =>
        map(f(c)){ b =>
          if(b) c +: l
          else l
        }
      }
    }

    // Using recursion
    def filterM2[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms match {
        case Nil => unit(Nil)
        case h :: t => flatMap(f(h))(b =>
          if (!b) filterM(t)(f)
          else map(filterM(t)(f))(h :: _))
      }
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A,B](gen: Gen[A])(f: A => Gen[B]): Gen[B] =
      gen flatMap f
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(pa)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](om: Option[A])(f: A => Option[B]) =
      om flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](la: List[A])(f: A => List[B]): List[B] =
      la flatMap f
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}
