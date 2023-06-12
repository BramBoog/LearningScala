import java.util.concurrent.TimeUnit

trait Callable[A]:
    def call: A

trait Future[A]:
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean

abstract class ExecutorService:
    def submit[A](a: Callable[A]): Future[A]

type Par[A] = ExecutorService => Future[A]

object Par:
    private case class UnitFuture[A](get: A) extends Future[A]:
        def isDone: Boolean = true
        def get(timeout: Long, unit: TimeUnit): A = get
        def isCancelled: Boolean = false
        def cancel(evenIfRunning: Boolean): Boolean = false

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) =>
            val af = a(es)
            val bf = b(es)
            UnitFuture(f(af.get, bf.get))

    def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable {def call = a(es).get})

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // Exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a, _) => f(a))

    // Exercise 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight(unit(Nil))((p, acc) => map2(p, acc)((a, as) => a :: as))

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
    }

    // Exercise 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
        val pars = as.map(asyncF(a => if (f(a)) List(a) else List()))
        map(sequence(pars))(_.flatten)
    }

    // Exercises page 110
    def parIndexedSeqFold[A, B](as: IndexedSeq[A])(z: B, f: A => B, g: (B, B) => B): Par[B] = fork {
        if (as.length <= 1) unit(as.headOption.fold(z)(f))
        else
            val (l, r) = as.splitAt(as.length/2)
            map2(fork(parIndexedSeqFold(l)(z, f, g)), fork(parIndexedSeqFold(r)(z, f, g)))(g)
    }

    def sumOfWords(strings: List[String]): Par[Int] =
        parIndexedSeqFold(strings.toIndexedSeq)(0, s => s.split('\n').map(_.split(' ')).flatten.length, _ + _)

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
        map2(map2(a, b)((a1, b1) => f.curried(a1)(b1)), c)((f1, c1) => f1(c1))

    /* Exercise 7.7
       given: map(y)(id) == y then map(y)(f) == f(y)
       map(map(y)(g))(f) == f(map(y)(g)) = f(g(y)) == f.compose(g)(y) == map(y)(f.compose(g))
    */

    // Exercise 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
        flatMap(n)(x => choices(x))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        flatMap(cond)(b => if (b) t else f)

    // Exercise 7.13
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
        es => f(pa(es).get)(es)

    // Exercise 7.14
    def join[A](a: Par[Par[A]]): Par[A] =
        es => a(es).get(es)

    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
        join(map(pa)(f))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
        flatMap(a)(identity)

    def map2ViaFlatMap[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        es =>
            flatMap(b)(b =>
                unit(flatMap(a)(a =>
                    unit(f.curried(a))
                )(es).get(b)))(es)
