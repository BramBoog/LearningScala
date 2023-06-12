case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
sealed trait Stream[+A]:

    // Exercise 5.1
    def toList: List[A] = 
        this match
            case Empty => Nil
            case Cons(h, t) => h() :: t().toList

    // Exercise 5.2
    def take(n: Int): Stream[A] =
        (this, n) match
            case (_, m) if (m <= 0) => sys.error("n has to be positive.")
            case (Empty, _) => Stream.empty
            case (Cons(h, t), 1) => Stream.cons(h(), Stream.empty)
            case (Cons(h, t), m) => Stream.cons(h(), t().take(m - 1))
        
    def drop(n: Int): Stream[A] =
        (this, n) match
            case (_, m) if (m <= 0) => sys.error("n has to be positive.")
            case (Empty, _) => Stream.empty
            case (Cons(h, t), 1) => t()
            case (Cons(h, t), m) => t().drop(m - 1)

    // Exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A] =
        this match
            case Empty => Stream.empty
            case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
            case Cons(h, t) => Stream.empty

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match
            case Empty => z
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
        
    // Exercise 5.4
    def forAll(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)

    // Exercise 5.5
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
        foldRight(Stream.empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

    // Exercise 5.6
    def headOption: Option[A] =
        foldRight(None: Option[A])((h, _) => Some(h))

    // Exercise 5.7
    def map[B](f: A => B): Stream[B] =
        foldRight(Stream.empty)((a, bs) => Stream.cons(f(a), bs))

    def filter(p: A => Boolean): Stream[A] =
        foldRight(Stream.empty)((a, as) => if (p(a)) Stream.cons(a, as) else as)
    
    def append[A1 >: A](s2: => Stream[A1]): Stream[A1] =
        foldRight(s2)((a, as) => Stream.cons[A1](a, as))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(Stream.empty)((a, bs) => f(a).append(bs))

    // Exercise 5.13
    def tail: Stream[A] =
        this match
            case Cons(h, t) => t()
            case Empty => sys.error("Cannot take the tail of an empty stream")        

    def mapViaUnfold[B](f: A => B): Stream[B] =
        Stream.unfold(this)(s => s.headOption.map(a => (f(a), s.tail)))

    def takeViaUnfold(n: Int): Stream[A] =
        if (n <= 0) sys.error("n has to be larger than 0")
        else Stream.unfold((this, n))((s, x) => s.headOption.flatMap(a => if (x > 0) Some(a, (s.tail, x - 1)) else None))

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
        Stream.unfold(this)(s => s.headOption.flatMap(a => if (p(a)) Some(a, s.tail) else None))

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
        Stream.unfold((this, s2))((sa, sb) => sa.headOption.flatMap(a => sb.headOption.map(b => (f(a,b), (sa.tail, sb.tail)))))

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
        Stream.unfold((this, s2))(
            (sa, sb) => (sa.headOption, sb.headOption) match
                case (None, None) => None
                case (Some(a), None) => Some(((Some(a), None), (sa.tail, Empty)))
                case (None, Some(b)) => Some(((None, Some(b)), (Empty, sb.tail)))
                case (Some(a), Some(b)) => Some(((Some(a), Some(b)), (sa.tail, sb.tail)))
        )

    // Exercise 5.14
    def startsWith[A1 >: A](s: Stream[A1]): Boolean =
        this.zipWith(s)((x, y) => x == y).forAll(identity)

    // Execerise 5.15
    def tails: Stream[Stream[A]] =
        Stream.unfold(this)(s => s match {case Empty => None case Cons(h, t) => Some((s, s.tail))}).append(Stream(Stream.empty))

    // Exercise 5.16
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
        // Cannot be done using unfold, because unfold builds the result from the head, instead of the last element like foldRight.
        // The following implementation is not linear, because in addition to traversing the stream once for each element,
        // it also has to do a lookup back into the constructed stream with headOption:
        // foldRight(Stream(z))((a, s) => Stream.cons(f(a, s.headOption.getOrElse(z)), s))
        // Instead we keep both the stream to return and its head element in state:
        foldRight((z, Stream(z)))(
            (a, acc) =>
                // acc is passed by name into this function and will be passed by-name in both f and cons.
                // So to avoid possible re-evaluation of the tuple should the user request to know both b and the tail, store it in a lazy val
                lazy val acc2 = acc
                val b = f(a, acc._1)
                (b, Stream.cons(b, acc._2))
        )._2

object Stream:
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // Exercise 5.8
    def constant[A](a: A): Stream[A] =
        cons(a, constant(a))

    // Exercise 5.9
    def from(n: Int): Stream[Int] =
        cons(n, from(n + 1))

    // Exercise 5.10
    def fibs: Stream[Int] =
        def loop(m: Int = 0, n: Int = 1): Stream[Int] =
            cons(m, loop(n, m + n))
        loop()

    // Exercise 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match
            case Some(a, s) => cons(a, unfold(s)(f))
            case None => empty
        
    // Exercise 5.12
    def fibsViaUnfold: Stream[Int] =
        cons(0, unfold((0, 1))((m, n) => Some((n, (n, m + n)))))

    def fromViaUnfold(n: Int): Stream[Int] =
        unfold(n)(x => Some((x, x + 1)))

    def constantViaUnfold[A](a: A): Stream[A] =
        unfold(a)(x => Some((x, x)))

    lazy val onesViaUnfold: Stream[Int] = constantViaUnfold(1)
