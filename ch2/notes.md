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

> _map_ helps establish an abstraction barrier that isolates the implementation of procedures that transform lists from the details of how the elements of the list are extracted and combined. This abstraction gives us the flexibility to change the low-level details of how sequences are implemented, while preserving the _conceptual_ framework of operations that transform sequences to sequences.
... It is able to be equally powerful to process the tree structure once the `map` is defined as an alias and being recursively called in the anonymous function within itself.


> Recursion is a natural tool for dealing with tree structures, since we can often reduce operations on trees to operations on their branches, which reduce in turn to operations on the branches of the branches, and so on, until we reach the leaves of the tree.

It also reveals a merit of functional programming. It is more about "procedure", which is the definition of what something is, and can be either _function_ or _data structure_. It is independent from the imperative programming, which is more of defining how something works. The former approach is easier to set abstraction barriers so that the program can have clear layers to expose complexity that is only relevant to the concepts in the same layer.

> The key to organizing programs so as to more clearly reflect the signal-flow structure is to concentrate on the "signals" that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages.
>
> The value of expressing programs as sequence operations is that this helps us make program designs that are modular, that is, designs that are constructed by combining relatively independent pieces. We can encourage modular design by providing a library of standard components together with a conventional interface for connecting the components in flexible ways.
>
> Modular construction is a powerful strategy for controlling complexity in engineering design. In real signal-processing applications, for example, designers regularly build systems by cascading elements selected from standardized families of filters and transducers. Similarly, sequence operations provide a library of standard program elements that we can mix and match.
>
> It is important to know how data abstraction permits us to design programs without becoming enmeshed in the details of data representations, and how abstraction preserves for us the flexibility to experiment with alternative representations. Another powerful design principle for working with data structures is the use of conventional interfaces.

....

> The key to organizing programs so as to more clearly reflect the signal-flow structure is to concentrate on the ``signals'' that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages.
>
> The value of expressing programs as sequence operations is that this helps us make program designs that are modular, that is, designs that are constructed by combining relatively independent pieces. We can encourage modular design by providing a library of standard components together with a conventional interface for connecting the components in flexible ways.
>
> Modular construction is a powerful strategy for controlling complexity in engineering design. In real signal-processing applications, for example, designers regularly build systems by cascading elements selected from standardized families of filters and transducers. Similarly, sequence operations provide a library of standard program elements that we can mix and match. _We can also formulate conventional data-processing applications in terms of sequence operations_.
>
> Sequences, implemented here as lists, serve as a conventional interface that permits us to combine processing modules. Additionally, _when we uniformly represent structures as sequences, **we have localized the data-structure dependencies in our programs to a small number of sequence operations**_. By changing these, we can **experiment with alternative representations of sequences, while leaving the overall design of our programs intact**.

Though data and procedures have a much obscure line in Lisp, this thought is actually useful to separate them apart. Usually, there is little control about the data that the program is receiving, as well as the format of data that would finally be presented. Both of them are API towards others (programs, services, or human beings). But it is also relatively well-defined about the operations.

_nit thing from racket_: The counterpart of `flat-map` at racket is `append-map`, which is to combine the mapping and accumulating with append. Actually, racket does a better job of naming. And even better, there is `append*` apart from `append`. The former operator `(append* lst ... lsts)` is equal to `(apply append lst ... lsts)`.

There are sequence of layers of language --

- Language of Schemes of Combinations
- Language of Geometric Positions
- Language of Primitive Pictures

In each level, the element that is defined is to talk about everything in this linguistic level, by using the items defined in the lower level. These levels at linguistics is much more robust than building a system with _tree_structure_, in which each node goes down to specific leaf node to do some specific job.

> _Some notes from the example of operating pictures_. It emphasizes
again about the importance of combination and abstraction. At each
stage, it is useful to use the "wishful thinking" to define the basic
elements of both data and procedures. The combination of data is to
build compound data with more complex inner structure, while it is
also possible to build higher-order procedure by _simply_ treat
procedures and data alike -- summarize an abstraction of procedure
with input of other lower-level procedures and leave out the "data" in
a lambda function, and then feed each input procedure the
corresponding data with this lambda. For example:

```scheme
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
```

> It is also a good example of how to define the problem and distinguish
complexities into different layers. A painting is divided into picture
and frame. For the painting, there are all kinds of rotations and
combinations. For a frame, it can be well defined by three
vectors. For a vector, we need to be able to add, subtract, and scale
them. On the other hand, for the simplest picture, it can be just
direct lines. Each line has a starting point and a end point....
This top-down approach makes the
definitions of lower level very clear -- what constructor, selectors
and operations are needed to support the current wishful thinking.

> The fundamental **data abstractions**, painters, are implemented using
procedural representations, which _enables the language to handle
different basic drawing capabilities in a uniform way_. The means of
**combination** satisfy the closure property, which _permits us to easily
build up complex designs_. Finally, all the tools for abstracting
procedures are available to us for _abstracting means of combination_
for painters.

> This is the approach of stratified design, the notion that a complex
system should be structured as a sequence of levels that are described
using a sequence of languages. Each level is constructed by combining
parts that are regarded as primitive at that level, and the parts
constructed at each level are used as primitives at the next
level. _The language used at each level of a stratified design has
primitives, means of combination, and means of abstraction appropriate
to that level of detail_.

_Quote of the lecture_: "Embedded something in the language is desirable, because you don't loose the virtue of the original language. LISP is a lousy language to do any particular job. But it is really powerful, so it is extremely useful to figure out the right language and to embed it into the LISP. This is the real power of this language."


## Lecture 6

It is reasonable to define problem by splitting it into multiple predicates (procedure). It makes the syntax easy to understand, and serves more of an interface for human understanding. Meanwhile, designing a program in a top-down level has a similar effect as TDD -- the lower-level representations are defined by interface first and then the implementation. This approach also set the abstraction barriers which makes the higher-level rules (derivatives in this lecture) independent from the implementation.

> Symbolic differentiation is of special historical significance in
Lisp. It was one of the motivating examples behind the development of
a computer language for symbol manipulation. Furthermore, it marked
the beginning of the line of research that led to the development of
powerful systems for symbolic mathematical work, which are currently
being used by a growing number of applied mathematicians and physicists.

> Since the differentiation program is defined in terms of abstract
data, we can modify it to work with different representations of
expressions solely by changing the predicates, selectors, and
constructors that define the representation of the algebraic
expressions on which the differentiator is to operate. It is made
possible by separate the algorithms from data representations via
building abstraction barriers (predicates, selectors, and constructors).

Some data structures are straightforward in the data representation,
while the other might not. `Set` is a valid example for the latter
case. In this scenario --
> the choice of a
representation is not so obvious. Indeed, there are a number of
possible representations, and they differ significantly from one
another in several ways. Informally, a set is simply a collection of
distinct objects. **To give a more precise definition we can employ the
method of data abstraction**. That is, we define ``set'' by specifying
the operations that are to be used on sets. These are union-set,
intersection-set, element-of-set?, and adjoin-set.

**Concerning the data representations**, the ordered lists are
beneficial in three folds compared with the unordered ones:

1. The search in any element within the list is O(lgN) (with the
   virtue from the binary tree)
2. The upper- and lower- bounds of the list are helpful to exclude
   items out of the range at the first place
3. Generate the set from list takes O(NlogN), while maintaining it
   only takes linear time, based on the assumption that the list is
   already in order. That is the case for both intersection and union


The advantage of the tree representation is this: if the tree is
"balanced", each of these subtrees will be about half the size of
the original. Thus, in one step we have reduced the problem of
searching a tree of size n to searching a tree of size n/2. We should
expect that the number of steps needed to search a tree of size n
grows as (log n).For large sets, this will be a significant speedup
over the previous representations. Information-retrieval systems in
which records have to be "randomly accessed" are typically
implemented by a tree-based method.

To maintain a roughly balanced tree structure, _one way_ is to define
an operation that **transforms an arbitrary tree into a balanced tree
with the same elements**. Then we can perform this transformation after
every few adjoin-set operations to keep our set in balance. There are
also _other ways_ to solve this problem, most of which involve **designing
new data structures for which searching and insertion both can be done
in (log n) steps**.

Quotation using at symbolic programming is a means to stop the
interpreter getting deeper. By doing so, it is able to build layers of
abstraction upon layers so that the language can be more powerful.


### _Side note for racket_

- `(cons 1 2)` returns a pair, and `(cons 1 '(2))` returns a list `'(1 2)`. The counterpart is that the `cdr` of a list is a **list** without the original leading element, while the `cdr` of a pair is the 2nd **element** of the pair.
- There are quite a few interesting _start_ functions for pair and list, such as `append*`. Its usage is `(append* lst ... lsts)`, and is equal to `(apply append lst ... lsts)`.
- There is not a `flatmap` as the example of "nested mappings" in the SICP, but there is a `append-map`, which is self-explanatory and equavelant to `(append* (map ...))`.

#### The differences among `eq?`, `eqv?`, and `equal?`

- `eq?` return #t if v1 and v2 refer to the same object, #f
- Two values are `eqv?` if and only if they are `eq?`, unless otherwise
specified for a particular datatype. The number and character datatypes are the only ones for which eqv? differs from `eq?`. Two numbers are `eqv?` when they have the same exactness, precision, and are both equal and non-zero, both `0.0`, both `0.0f0`, both `-0.0`, both `-0.0f0`, both `+nan.0`, or both `+nan.f`—considering real and imaginary components separately in the case of complex numbers. Two characters are `eqv`? when their `char->integer` results are equal.
- Two values are `equal?` if and only if they are `eqv?`, unless otherwise specified for a particular datatype. Datatypes with further specification of `equal?` include strings, byte strings, pairs, mutable pairs, vectors, boxes, hash tables, and inspectable structures. In the last six cases, equality is recursively defined; if both v1 and v2 contain reference cycles, they are equal when the infinite unfoldings of the values would be equal. See also `gen:equal+hash` and `prop:impersonator-of`.


### Miscellaneous

In general, we can attain significant savings if we use variable-length
prefix codes that take advantage of the relative frequencies of the symbols
in the messages to be encoded. One particular scheme for doing this is
called the Huffman encoding method.

A Huffman code can be represented as a binary tree whose leaves are
the symbols that are encoded. At each non-leaf node of the tree there
is a set containing all the symbols in the leaves that lie below the
node. In addition, each symbol at a leaf is assigned a weight (which
is its relative frequency), and each non-leaf node contains a weight
that is the sum of all the weights of the leaves lying below it. The
weights are not used in the encoding or the decoding process, but are
used to help construct the tree.
