package Chapter4
import Chapter3.*

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A]:
    // Exercise 4.1
    def map[B](f: A => B): Option[B] =
        this match
            case None => None
            case Some(a) => Some(f(a))

    def flatMap[B](f: A => Option[B]): Option[B] =
        this.map(f).getOrElse(None)
    
    /* B >: A indicates B is the same type as or a supertype of A.
    default: => B means that the argument is of type B, but won't be evaluated until
    it's needed by the function (non-strictness/lazyness). */
    def getOrElse[B >: A](default: => B): B =
        this match
            case None => default
            case Some(get) => get
        
    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
        this.flatMap(a => if(f(a)) Some(a) else None)

    // Exercise 4.2
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

object Option:
    // Exercise 4.3
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
        a.flatMap(aa => b.map(bb => f(aa, bb)))

    // Exercise 4.4
    def sequence[A](as: List[Option[A]]): Option[List[A]] =
        List.foldRight[Option[A], Option[List[A]]](as, Some(Nil))((a, acc) => map2(a, acc)((a,l) => Cons(a,l)))

    // Exercise 4.5
    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        List.foldRight[A, Option[List[B]]](as, Some(Nil))((a, acc) => map2(f(a), acc)((b,l) => Cons(b,l)))

    def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
        traverse[Option[A], A](as)(identity)

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
sealed trait Either[+E, +A]:
    // Exercise 4.6
    def map[B](f: A => B): Either[E, B] =
        this match
            case Left(e) => Left(e)
            case Right(a) => Right(f(a))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match
            case Right(a) => f(a)
            case Left(e) => Left(e)
        
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match
            case Right(a) => Right(a)
            case Left(_) => b

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        this.flatMap(aa => b.map(bb => f(aa, bb)))

object Either:
    // Excercise 4.7
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        List.foldRight[A, Either[E, List[B]]](as, Right(Nil))((a, acc) => f(a).map2(acc)((b, l) => Cons(b,l)))

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        traverse[E, Either[E, A], A](es)(identity)
