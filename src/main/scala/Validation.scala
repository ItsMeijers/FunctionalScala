sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation{
  def validationApplicate[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def apply[A, B](fab: Validation[E,A => B])(f: Validation[E,A]): Validation[E,B] =
      f match {
        case Success(a) => fab match {
          case Success(f) => Success(f(a))
          case Failure(h, t) => Failure(h, t)
        }
        case Failure(h, t) => fab match {
          case Failure(h2, t2) => Failure(h, (h2 +: t) ++ t2)
          case _ => Failure(h,t)
        }
      }

    def unit[A](a: => A): Validation[E,A] = Success(a)
  }
}
