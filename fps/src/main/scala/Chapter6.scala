trait RNG:
    def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)

// Exercise 6.1
def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    val n1 =
        if (n < 0) -(n + 1)
        else n
    (n1, rng2)

// Exercise 6.2
def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), rng2)

// Exercise 6.3
def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)

def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)

// Exercise 6.4
def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0) (List(), rng)
    else
        val (n, rng2) = rng.nextInt
        val (ns, rng3) = ints(count - 1)(rng2)
        (n :: ns, rng3)

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

// Example of a (very basic) state action/transformation
def unit[A](a: A): Rand[A] =
    rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
        val (a, rng2) = s(rng)
        (f(a), rng2)

// Excercise 6.5
val doubleViaMap: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

// Exercise 6.6
// Example of a combinator, combining two state actions
def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a, b), rng3)

// Exercise 6.7
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))((f, randL) => map2(f, randL)((a, as) => a :: as))
    
def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

// Exercise 6.8
def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>
        val (a, rng2) = f(rng)
        g(a)(rng2)

def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i =>
        rng =>
            val mod = i % n
            if (i + (n-1) - mod >= 0) (mod, rng)
            else nonNegativeLessThan(n)(rng)
    )

// Exercise 6.9
def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

// Exercise 6.10
/** General state action data type */
case class State[S, +A](run: S => (A, S)):
    def map[B](f: A => B): State[S, B] =
        State(
            s =>
                val (a, s1) = run(s)
                (f(a), s1)
        )

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
        State(
            s =>
                val (a, s1) = run(s)
                sb.map(f.curried(a)).run(s1)
        )
    
    def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(
            s =>
                val (a, s1) = run(s)
                f(a).run(s1)
        )

object State:
    def unit[S, A](a: A): State[S, A] =
        State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
        fs.foldRight(unit(Nil: List[A]))((f, sLst) => f.map2(sLst)((a, as) => a :: as))

    /** Get the incoming state by passing it along as the value (as well as as the state). */
    def get[S]: State[S, S] = State(s => (s, s))

    /** Set the state to s, ignoring the incoming state. */
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    /** Modify the incoming state by f. Return Unit as the value to indicate it doesn't return anything other than the new state. */
    def modify[S](f: S => S): State[S, Unit] =
        for
            s <- get
            _ <- set(f(s))
        yield ()

// Exercise 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int):
    private def insertCoin: Machine =
        if (candies <= 0 || !locked) this
        else this.copy(locked = false, coins = coins + 1)

    private def turnKnob: Machine =
        if (candies <= 0 || locked) this
        else this.copy(locked = true, candies = candies - 1)
    
    def takeInput(i: Input): Machine =
        i match
            case Coin => insertCoin
            case Turn => turnKnob

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for 
        _ <- State.sequence(inputs.map(i => State.modify[Machine](m => m.takeInput(i))))
        s <- State.get
    yield (s.coins, s.candies)
