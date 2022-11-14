package part1

object Chapter2 extends App:

    // Exercise 2.1
    def fib(n: Int): Int =
        def loop(i: Int, x0: Int, x1: Int): Int =
            if (i == n) x1
            else loop(i+1, x1, x0+x1)

        if (n == 0) 0
        else loop(1, 0, 1)

    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(3) == 2)
    assert(fib(5) == 5)

    // Exercise 2.2
    def isSorted[A](ar: Array[A], ordered: (A,A) => Boolean): Boolean =
        def loop(n: Int): Boolean =
            if (n >= ar.length) true
            else if (!ordered(ar(n-1), ar(n))) false
            else loop(n+1)
        
        loop(1)
    
    assert(isSorted(Array(4,3,2,1), (x: Int, y: Int) => x > y) == true)
    assert(isSorted(Array(2,3,1,4), (x: Int, y: Int) => x > y) == false)
    assert(isSorted(Array(1,2,3,4), (x: Int, y: Int) => x < y) == true)
    assert(isSorted(Array(2,3,1,4), (x: Int, y: Int) => x < y) == false)
    assert(isSorted(Array("no way", "hello", "hey"), (x: String, y: String) => x.length > y.length) == true)
    assert(isSorted(Array("hey", "hello", "no way", "what"), (x: String, y: String) => x.length > y.length) == false)

    // Exercise 2.3
    def curry[A,B,C](f: (A,B) => C): A => (B => C) =
        (a: A) => f(a, _)

    val addCurried: Int => (Int => Int) = curry((x: Int, y: Int) => x+y)
    val add1 = addCurried(1)
    
    assert(add1(2) == 3)
    assert(add1(3) == 4)

    // Exercise 2.4
    def uncurry[A,B,C](f: A => B => C): (A,B) => C =
        (a, b) => f(a)(b)
    
    assert(uncurry(addCurried)(1,2) == 3)

    // Exercise 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        a => f(g(a))

    val add1ToString: Int => String = compose(_.toString, x => x + 1)

    assert(add1ToString(2) == "3")
    