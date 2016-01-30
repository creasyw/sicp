## Lecture 4

> A procedure can be regarded as a pattern for the local evolution of a process. Higher-order procedures enhance the power of our language by enabling us to manipulate, and thereby to reason in terms of, general methods of computation. **This is much of the essence of programming**. A procedure used as an element in creating a more complex procedure could be regarded not only as a collection of particular operations but also as a procedural abstraction.
> 
> Another key aspect of any programming language: the means it provides for building abstractions by combining data objects to form compound data. For the same reasons that we want compound procedures: to elevate the conceptual level at which we can design our programs, to increase the modularity of our designs, and to enhance the expressive power of our language. Just as the ability to define procedures enables us to deal with processes at a higher conceptual level than that of the primitive operations of the language, the ability to construct compound data objects enables us to deal with data at a higher conceptual level than that of the primitive data objects of the language.
> 
> The main issue to be addressed is that of abstraction as a technique for coping with complexity, and data abstraction enables us to erect suitable abstraction barriers between different parts of a program. One key idea in dealing with compound data is the notion of closure -- that the glue we use for combining data objects should allow us to combine not only primitive data objects, but compound data objects as well. Another key idea is that compound data objects can serve as conventional interfaces for combining program modules in mix-and-match ways.
> 
> Just as a given numerical function can be computed by many different computational processes, there are many ways in which a given data structure can be represented in terms of simpler objects, and the choice of representation can have significant impact on the time and space requirements of processes that manipulate the data.
> 
> Data may be represented differently by different parts of a program. This leads to the need to implement _generic operations_, which must handle many different types of data. Maintaining modularity in the presence of generic operations requires more powerful abstraction barriers than can be erected with simple data abstraction alone. In particular, the data-directed programming as a technique allows individual data representations to be designed in isolation and then combined additively (i.e., without modification).


The approach for setting abstraction barriers between primitive (or maybe other compound) data and the conceptual mathematical manipulations is called "_data abstraction_", in terms of adding constructors and selectors for the proposed compound data.

> The underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.

The detailed implementation of the "pair" `cons` is the first time in this book it blurs the boundary between data and procedure. What the ... ?!

```
(define (cons a b)
  (lambda (pick)
    (cond ((= pick 1) a)
          ((= pick 2) b))))

(define (car x) (x 1))
(define (cdr x) (x 2))
```

- It is able to implement an abstract concept (pair) in a abstract way
- It blurs the boundary between data and procedure. Procedure here is concrete entity -- it is an object.

_Quote of the lecture_: "the naming at programming is the same as the naming at sorcery -- if you name it, you own it".

## Lecture 5


`for-each` only cares about doing something to each element of the list but not really cares about building a new list and/or storing the generated items. On the contrary, `map` is to generate a new list _by applying the procedure to each element in the original list_.

The methodology of data abstraction is to isolate the way that the data is used from the way that the data is represented. "The means of combination" in Lisp is **closed** -- it can be `cons` of `cons`, as well as `list` of `list`. The "**closure**" property would incorporate complexity really fast.

The ability to create pairs whose elements are pairs is the essence of
list structure's importance as a representational tool. It is being
referred as the "_closure property_" of cons. In general, an operation
for combining data objects satisfies the closure property if the
results of combining things with that operation can themselves be
combined using the same operation.6 Closure is the key to power in any
means of combination because it permits us to create hierarchical
structures -- structures made up of parts, which themselves are made
up of parts, and so on.

> Procedural representations of data will play a central role in our programming repertoire and this style of programming is often called _message passing_.

This "_procedure_" is important for the _closure_ property. Take the drawing picture at 2.4 as an example, the `picture` procedure does not care if the picture is a point, segment, or a real picture. It only cares that there is something that will be drawn to a coordinate. The _means of combinations_ are procedures, so they are inherently closed.

_map_ helps establish an abstraction barrier that isolates the
implementation of procedures that transform lists from the details of
how the elements of the list are extracted and combined. Like the
barriers shown in figure 2.1, this abstraction gives us the
flexibility to change the low-level details of how sequences are
implemented, while preserving the conceptual framework of operations
that transform sequences to sequences.

There are sequence of layers of language --

- Langage of Schemes of Combinations
- Lanugage of Geometric Positions
- Lanugage of Primitive Pictures

In each level, the element that is defined is to talk about everything in this linguistic level, by using the items defined in the lower level. These levels at linguistics is much more robust than building a system with _tree_structure_, in which each node goes down to specific leaf node to do some specific job.

_Quote of the lecture_: "Embeded something in the language is desireable, because you don't loose the virtue of the original language. LISP is a lousy language to do any particular job. But it is really powerful, so it is extremely useful to figure out the right language and to embed it into the LISP. This is the real power of this language." 
