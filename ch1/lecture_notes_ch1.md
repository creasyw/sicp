The notes are an excerpt from the course [Structure and Interpretation of Computer Programs](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/)

## Lecture 1
Describe "how to" knowledge

- rules for writing compound expressions -- syntax
- rules for assigning meaning to constructs -- semantics
- rules for capturing process of evaluations -- procedures

Use procedure to control the complexity:

- primitive elements in language
- rules for combining elements (means of combination)
- rules for abstracting elements (means of abstraction)

All of the three procedures allow to create complex procedure while suppressing details.

## Leture 2

"The key point to handle the complexity is to understand where to look at and which part of the system should be ignored and considered as primitive (not to look at)."

`(IF <predicate> <consiquence> <alternative>)`

Two algorithms performing adding:

_Interation_ with `O(x)` time and `O(1)` space:
```
(define (+ x y)
  (+ (-1 + x) (1 + y)))
```
(assuming the prefix procedure is what needs to be defined and the infix is the primitive precedures)

_Linear recursion_ with `O(x)` time and `O(x)` space:
```
(define (+ x y)
  (1+ (+ (-1 + x) y)))
``` 

Tree recursion for computing fibonacci number
```
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2)))))
```
This is a terrible recusion in the means of time/space complexity: for a large number of `n`, most of the nodes in the trees are actually calculated multiple times. Time complexity is `O(fib(n))`, and space is `O(n)` (the deepest line of the tree procedure).

The analogy of "**[Tower of Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi)**" is so brilliant!!

```
(define (Move N From To Spare)
  (cond ((= N 0) `Done)
        (else
          (Move (- N 1) From Spare To)
          (Move Last-one From To Spare)
          (Move (- N 1) Spare To From))))
```
It has an _almost_ exactly tree structure to solve the puzzle as what Fibonacci does, which takes _exponential_ time and _linear_ space.

## Lecture 3

A general form of _sigma_ adding is:

```
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
```
in which two precedures `term` and `next` are fed into the procudre `sum`, so that the `sum` would know how to handle the data in each step and how to derive the _next_ data, as well as when to end.

```
(define (sqrt x)
  (fixed-point
    (average_damping (lambda (y) (/ x y)))
    1))

(define average-damping
  (lambda (f)
    (lambda (x) (average (f x) x))))
```

The first argument `f` for the `average-damping` is a syntax sugar as `(define (average-damping f) ..)`. Either this way or the way in the code block would generate a procedure that is able to feed into the procedure `fixed-point`, which receives a procedure and a number as starting point. Still talking about the `average-damping`, the two `lambda` make a good example of [Currying](https://en.wikipedia.org/wiki/Currying).

The "fixed point" is an essential function in this lecture. It uses iteration to find a solution close enough for the function `f(x) = x`. In other words, it is to solve the problem of [Recurrence Equation](http://mathworld.wolfram.com/RecurrenceEquation.html). So, both solutions of finding fibonacci sum and solving square root via [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method) are using the estimate of fixed point.

The rights and privileges of the 1st-class citizens:

- to be named by variables
- to be passed as arguments to procedures
- to be returned as values of procedures
- to be incorporated into data structures

> Compound procedures are a crucial abstraction mechanism, because they permit us to express general methods of computing as explicit elements in our programming language. 
>
>As programmers, we should be alert to opportunities to identify the underlying abstractions in our programs and to build upon them and generalize them to create more powerful abstractions.... The significance of higher-order procedures is that they enable us to represent these abstractions explicitly as elements in our programming language, so that they can be handled just like other computational elements.
>
>Lisp awards procedures full first-class status. This poses challenges for efficient implementation, but the resulting gain in expressive power is enormous.

_Quote of this lecture_ -- "'[Wishful thinking](https://en.wikipedia.org/wiki/Wishful_thinking)' is essential to good engineering, and is certainly also essential to good computer science."

