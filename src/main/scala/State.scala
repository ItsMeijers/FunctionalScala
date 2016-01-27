import State._

case class State[S, +A](run : S => (A, S)){
  //type State[S, +A] = S => (A, S)
  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s:S) => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })

  def map[B](f: A => B) = this.flatMap(a => unit(f(a)))

  def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = for{
    a <- this
    b <- that
  } yield(f(a, b))

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]())){(c, acc) =>
      c flatMap(a => acc.map(l => a +: l))
    }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for{
    s <- get
    _ <- set(f(s))
  } yield ()

}
