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

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
