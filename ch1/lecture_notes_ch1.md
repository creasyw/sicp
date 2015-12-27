The notes are an excerpt for the MIT open course [Structure and Interpretation of Computer Programs](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/)

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