sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List:
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2
    def tail[A](l: List[A]): List[A] = l match
        case Nil => ???
        case Cons(h, t) => t

    def head[A](l: List[A]): A = l match
        case Nil => ???
        case Cons(h, _) => h

    // Exercise 3.3
    def setHead[A](el: A, l: List[A]): List[A] = l match
        case Nil => Nil
        case Cons(_, t) => Cons(el, t)

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] =
        (l, n) match
            case (Nil, _) => Nil
            case (Cons(_, t), 1) => t
            case (Cons(_, t), m) => drop(t, m-1)

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
        l match
            case Nil => Nil
            case Cons(h, t) if f(h) => dropWhile(t, f)
            case _ => l

    // Exercise 3.6
    def init[A](l: List[A]): List[A] =
        l match
            case Nil => Nil
            case Cons(_, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))

    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
        as match
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))

    // Exercise 3.9
    def length[A](l: List[A]): Int =
        foldRight(l, 0)((x,y) => 1+y)

    // Exercise 3.10
    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
        as match
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(z, x))(f)

    // Exercise 3.11
    def sum(l: List[Int]): Int =
        foldLeft(l, 0)(_+_)

    def product(l: List[Double]): Double =
        foldLeft(l, 1.0)(_*_)

    def lengthViaFoldLeft[A](l: List[A]): Int =
        foldLeft(l, 0)((x,y) => x+1)

    // Exercise 3.12
    def reverse[A](l: List[A]): List[A] =
        foldLeft(l, Nil: List[A])((lst, el) => Cons(el, lst))

    // Exercise 3.13
    def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B =
        foldRight(List.reverse(as), z)((x,y) => f(y,x))

    def foldRightViaFoldLef[A,B](as: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(List.reverse(as), z)((x,y) => f(y, x))

    // Exercise 3.14
    def append[A](l1: List[A], l2: List[A]): List[A] =
        foldRight(l1, l2)((el, lst) => Cons(el,lst))

    // Exercise 3.15
    def concat[A](l: List[List[A]]): List[A] =
        foldLeft(l, Nil: List[A])(append)

    // Exercise 3.16
    def add1ToEach(l: List[Int]): List[Int] =
        foldRight(l, Nil: List[Int])((i, lst) => Cons(i + 1, lst))

    // Exercise 3.17
    def eachDoubleToString(l: List[Double]): List[String] =
        foldRight(l, Nil: List[String])((d, lst) => Cons(d.toString, lst))

    // Exercise 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((x, xs) => Cons(f(x), xs))

    // Exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        as match
            case Nil => Nil
            case _ => foldRight(as, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
    
    // Exercise 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        concat(map(as)(f))

    // Exercise 3.21
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if (f(a)) List(a) else Nil)

    // Exercise 3.22
    def addElementWise(l1: List[Int], l2: List[Int]): List[Int] =
        (l1, l2) match
            case (Nil, Nil) => Nil
            case (Nil, Cons(h, t)) => throw new Exception("The second list is longer than the first.")
            case (Cons(h, t), Nil) => throw new Exception("The first list is longer than the second.")
            case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addElementWise(t1, t2))

    // Exercise 3.23
    def zipWith[A, B, C](l1: List[A], l2: List[B], op: (A, B) => C): List[C] =
        (l1, l2) match
            case (Nil, Nil) => Nil
            case (Nil, Cons(h, t)) => throw new Exception("The second list is longer than the first.")
            case (Cons(h, t), Nil) => throw new Exception("The first list is longer than the second.")
            case (Cons(h1, t1), Cons(h2, t2)) => Cons(op(h1, h2), zipWith(t1, t2, op))

    // Exercise 3.24
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
        if (length(sup) < length(sub)) throw new IllegalArgumentException("The length of the sublist is longer than the list to search.")
        else
            def makeSliding(inputL: List[A]): List[List[A]] =
                def makeSlidingSub(l: List[A], i: Int): List[A] =
                    if (i == length(sub)) Nil
                    else Cons(head(l), makeSlidingSub(tail(l), i+1))
                
                if (length(inputL) < length(sub)) Nil
                else Cons(makeSlidingSub(inputL, 0), makeSliding(tail(inputL)))
            
            length(dropWhile(makeSliding(sup), _!=sub)) > 0

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Exercise 3.25
def size[A](t: Tree[A]): Int =
    t match
        case Leaf(a) => 1
        case Branch(l, r) => 1 + size(l) + size(r)

// Exercise 3.26
def maximum(t: Tree[Int]): Int =
    t match
        case Leaf(a) => a
        case Branch(l, r) => maximum(l).max(maximum(r))

// Exercise 3.27
def depth[A](t: Tree[A]): Int =
    t match
        case Leaf(a) => 1
        case Branch(l, r) => 1 + depth(l).max(depth(r))

// Exercise 3.28
def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l, f), map(r, f))

// Exercise 3.29
def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B =
    t match
        case Leaf(a) => f(a)
        case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))

def sizeViaFold[A](t: Tree[A]): Int =
    fold(t, _ => 1, (l, r) => 1 + l + r)

def maximumViaFold(t: Tree[Int]): Int =
    fold(t, identity, (l, r) => l.max(r))

def depthViaFold[A](t: Tree[A]): Int =
    fold(t, _ => 1, (l, r) => 1 + l.max(r))

def mapViaFold[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold[A, Tree[B]](t, a => Leaf(f(a)), (l, r) => Branch(l, r))
