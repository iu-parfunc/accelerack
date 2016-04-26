#lang scribble/manual

@(require (for-label (except-in racket map sqrt)
                     accelerack
                     rackunit
                     2htdp/image
                     )
#;    scribble/eval
    rackunit
    scribble/examples
    )

@title{Data-Parallel Programming with Accelerack (v0.4)}

In this course we are using a library called @racket[accelerack],
which allows you to use Racket for data-parallel programming
over multidimensional arrays.
@; RRN: Add this when it works:
@; for GPUs.

All programs that use @racket[accelerack], need only a single import:

@defmodule[accelerack]

Once you have done that, some of the functions you are familiar with
(such as @racket[map]) have been replaced with expanded versions.
And you have access to a host of new functions for manipulating arrays.

Finally, as described in @secref["typed"], Accelerack is actually its own
@emph{sublanguage}, complete with a type-checker and separate compiler for a
subset of the normal Racket language.  This arrangement is sometimes called an
"embedded domain specific language" (EDSL).

@; -------------------------------------------------------
@section[#:tag "arrays"]{Array datatype and properties}

@defproc[(acc-array [acc-element acc-element?] ...) acc-array?]

Arrays are collections of values. Unlike lists, they are fixed in
size, and can be indexed efficiently, in constant time.

The @racket[acc-array] form introduces a literal array, just as "hello" and 39
introduce literal strings and integers, respectively.  These arrays can be zero
dimensional, one dimensional, two dimensional or more.

@racketblock[
(define-acc x (acc-array 1))
(define-acc y (acc-array (1 2 3)))
(define-acc z (acc-array ((1 2 3)
                          (4 5 6))))
(check-true (acc-array? x))
(check-true (acc-array? y))
(check-true (acc-array? z))
]

The dimension can be computed with @racket[acc-array-dimension], and also
corresponds to the number of parentheses before the first data element.

The @racket[acc-array] datatype represents @emph{regular} multi-dimensional
arrays.  Thus all the rows of a two-dimensional array (matrix) must be the same
length.


@defproc[(acc-array? [array any]) boolean?]

Determine whether a value is an @racket[acc-array].

@defproc[(acc-array-dimension [acc-array acc-array?])
         integer?]

Return the number of dimensions of an array.

@defproc[(acc-array-shape [acc-array acc-array?])
         (vectorof integer?)]

Returns a vector of the sizes of each dimension of an array.
Note that the shape returned satisfies @racket{acc-element?}, and also
@racket{acc-shape?}.

@examples[(require accelerack)
          (acc-array-shape (acc-array ((1 2 3) (4 5 6))))
          ]

Note that the rightmost component of the shape, is the length of the innermost
list in the array literal, e.g., @racket[(1 2 3)].

@defproc[(acc-shape? [x any]) boolean?]

Determine whether a value is a valid shape, i.e. a vector of zero or more non-negative integers.

@racketblock[
(check-true (acc-shape? #()))
(check-true (acc-shape? #(100)))
(check-true (acc-shape? #(100 100)))
]

@defproc[(acc-array-size [acc-array acc-array?])
         integer?]

Returns the size of an array.  That is, the number of total elements.  This is equal to the product of the sizes in each dimension, that is:

@racketblock[
(check-equal?
   (acc-array-size a)
   (apply * (vector->list (acc-array-shape a))))
]

@; -------------------------------------------------------
@section[#:tag "arrays"]{Array elements and element-wise access}

@defproc[(acc-element? [element any]) boolean?]

Determine whether a value can go inside an @racket[acc-array], that is, whether it can be an
array @emph{element}.  This currently includes: exact integers, inexact floating point numbers, and booleans.
Also, vectors of elements are valid elements (including vectors of vectors).

@defproc[(acc-array-ref [acc-array acc-array?] 
                        [index integer?] ...) acc-element?]

To reference an element in an array, you provide its multidimensional @emph{index}. The
@racket[acc-array-ref] function takes as many index parameters
as there are dimensions in the input array.

For example, for a 3D array @racket[arr] with shape @racket[(vector x y z)], the
smallest and largest valid indices are
@racket[(acc-array-ref arr 0 0 0)] and
@racket[(acc-array-ref arr (sub1 x) (sub1 y) (sub1 z))].
These are diagonally opposite "corners" of the volume.

@defproc[(acc-array-flatref [acc-array acc-array?]
                            [flat-index integer?]) acc-element?]

Like @racket[acc-array-ref], this function retrieves an element from
an array using an index. Unlike @racket[acc-array-ref], the @racket[acc-array-flatref]
function only takes @emph{one} integer as the index. This integer is the
 index of the element in a flattened, "row-major order" view of the array.
Valid indices are thus between 0 and @racket[acc-array-size] minus 1.

Note, currently, row-major is in fact the order acc-array elements are stored
in memory, so the @racket[acc-array-flatref] index corresponds directly to the
index in memory.


@; -------------------------------------------------------
@section[#:tag "arrays"]{Array conversions}

@defproc[(acc-array->sexp [acc-array acc-array?]) sexp?]

Arrays can be converted into ordinary Racket s-expressions.
The result is a nested series of lists, with nesting depth equal to the dimension of the array.
The element values will all satisfy @racket[acc-element?].

Note that the s-expression uses the same format as the @racket[acc-array] syntax for constructing literal arrays.

@racketblock[
(define x (acc-array 15))
(acc-array->sexp x)
]

@; TODO: sexp->acc-array

@defproc[(sexp->acc-array [sexp sexp?]) acc-array?]

Converts Racket s-expressions to acc-arrays where sexps
The input should consist of nested series of lists, with nesting depth equal to the dimension of the array.
The element values should satisfy @racket[acc-element?]

@racketblock[
(sexp->acc-array '1)
(sexp->acc-array '(1 2 4))
(sexp->acc-array '(#(1) #(2)))
]

Note that the s-expression uses the same format as
the @racket[acc-array] syntax for constructing literal arrays.


@; -------------------------------------------------------
@section[#:tag "functions"]{Building Arrays}

Aside from typing literal arrays into the program using the @racket[acc-array]
syntax, we also want to programatically generate arrays of arbitrarily large
size, for which we use @racket[generate].

@defproc[(generate [proc procedure?] [l exact-nonnegative-integer?] ...) acc-array?]

Here the function @racket[proc] generates one element of the array at time.  It
takes @racket[N] arguments, specifying the exact position element being
generated in an @racket[N]-dimensional space.

To determine the overall size of the array, we also need @racket[N] length
arguments, @racket{l ...}.  For example, the following would produce a
100-element 1D array, followed by a 10,000 element 2D array.

@racketblock[
(define oneD (generate (lambda (r  ) r) 100))
(define twoD (generate (lambda (r c) r) 100 100))
]

@; -------------------------------------------------------
@section[#:tag "functions"]{Operating on Arrays}

@defproc[(map [proc procedure?] [acc-array acc-array?]) acc-array?]

Mapping over an array is like mapping over a list, but it can be done
in parallel and across multiple dimensions.

@racketblock[
(define x (acc-array (1 2 3)))
(define y (map (lambda (x) (+ x 1)) x))
; (acc-array (2 3 4))
]

@defproc[(fold [proc procedure?] [init acc-element?] [acc-array acc-array?]) acc-array?]

Folding also works on arrays similarly to how it works on lists. Importantly,
because the fold is now parallel, there is no distinction between left and
right fold. The function may be applied to the elements of the array in any order.

@defproc[(zipwith [proc procedure?] [acc-array1 acc-array?] [acc-array2 acc-array?]) acc-array?]

@racket[zipwith] is just @racket[map] over two input arrays. The function is expected
to take two arguments.


@section[#:tag "functions"]{Stencils and stencil boundary conditions}

@defproc[(stencil3x3 [proc procedure?] [b stencil-boundary?] [a acc-array?]) acc-array?]

Run a 3x3 stencil over a two-dimensional array.  The output array is the same
size as the input array.  In this way @racket[stencil3x3] is like a map, but
whereas only one value is passed to @racket[proc] in the case of @racket[map],
here the @racket[proc] procedure observes a @emph{neighborhood} of values
around the point in question.

Thus @racket[proc] is a nine argument function that takes a 3x3 region of
elements.  In the following example we simple return the middle value, which
makes the stencil behave just like a map:

@racketblock[
(stencil3x3 (lambda (x1 x2 x3
                     x4 x5 x6
                     x7 x8 x9) x5)
  '(Constant 0) x)
]

The boundary condition argument specifies what happens when the neighborhood
includes points outside the image.  In this example, if @racket[x5] is at an
"upper-left" corner of the 2D array, then @racket[x1], @racket[x2],
@racket[x3], @racket[x4], and @racket[x7] would all be @racket[0] based on the
boundary condition.

@defproc[(stencil-boundary? [x any]) boolean?]

Currently, valid boundary conditions have the form:

@racketblock[
@; 'Clamp
@; 'Mirror
@; 'Wrap
'(Constant v)
]

where @racket[(acc-element? v)].

@; TODO: replicate

@; TODO: until

@; -------------------------------------------------------
@section[#:tag "functions"]{Image functions}

The @racket[image] type from @racket[2htdp/image] can be converted too
and from lists, but @racket[acc-array]'s are much more efficient than lists.

@defproc[(image->acc-array [x image?]) acc-array?]

@defproc[(acc-array->image [x acc-array?]) image?]

In these conversions, a two-dimensional image maps onto a two dimensional array.

We keep the order of indices the same, for ease of interoperability.
That is, a point @racket[(x,y)] in the original image, will be
referenced by @racket[(acc-array-ref arr x y)] in the converted array.  Thus:

@racketblock[
(equal? (acc-array-shape (image->acc-array img))
        (vector (image-width img)
                (image-height img)))
]

Finally, when performing the conversion, we convert the struct @racket[color]
(which contains red, green, blue, alpha) into an equivalent vector that satisfies
@racket[acc-element?]: @racket[(vector r g b a)].

The following functions provide convenient conversions:

@defproc[(color->acc-element [x color?]) acc-element?]

@defproc[(acc-element->color [x (vector integer? integer? integer? integer?)])
         color?]

Note that @racket[acc-element->color] does not work for @emph{any}
element, rather it must specifically be a vector of four integers
between 0 and 255.


@; -------------------------------------------------------
@section[#:tag "typed"]{Typed Accelerack Computations}

Because Accelerack is an embedded language, there are designated ways of
telling the system "now I'm about to write Accelerack" code.

@defform[(acc expr)]

For example, @racket[(acc 3)] and @racket[3] evaluate to exactly the same
thing, but the former is engaging the restricted Accelerack language, and is
thus type-checked.  Thus @racket[(acc (+ 1 #t))] will result in an error before
it ever runs, whereas @racket[(+ 1 #t)] will only create an error when it is
actually executed, e.g. when the function it resides in is called.

@; --------------------------------------------------------------
@subsection[#:tag "typed"]{Types and Annotations}

What are the valid types in Accelerack?  To start with, there are @racket[Int],
@racket[Double], and @racket[Bool]---the most basic types kinds of Accelerack
data.  Even though the Accelerack compiler will infer types for us, we can
double check that we have the right type by annotating explicitly with a
@racket[(: expr type)] form.

@defform[(: expr type)]

@examples[
 (require accelerack)
 (acc (: 3   Int))
 (acc (: 3.3 Double))
 (acc (: #t  Bool))
]

Note that how we write numbers affects their type.  @racket[3.0] is a
@racket[Double] whereas @racket[3] is an @racket[Int].

In addition to directly evaluating Accelerack code with @racket[(acc ...)] we
can also create top-level definitions in the Accelerack language.

@defform*[((define-acc (name args ...) body)
           (define-acc name expr))]

Define an Accelerack computation, either a function or expression.  The value
defined by @racket[name] is usable in regular Racket code as well as in other
Accelerack expressions.

@examples[(require accelerack)
          (define-acc x 3)
          x
          (acc x)
          (define y 4)
          y
          (eval:error (acc y))]

Here we can see that the reverse is not true---normal Racket variables cannot
be used in Accelerack computations.  There is, however, a way to @emph{import}
such values, if we can assign them a valid Accelerate type.  We describe this
in the next section, @secref["importing"].

There are some additional restrictions on @racket[define-acc] definitions
compared to regular @racket[define].  In addition to only supporting a limited
set of constructs, @emph{recursive} definitions are not permitted.

As a final note on @racket[define-acc], observe that @emph{args} can consist
not just of variables, but of type annotated variables @racket[(v : t)], for
example:

@examples[(require accelerack)
          (define-acc (f [x : Int]) (+ x 3))

          (define-acc (g x) (+ (: x Int) 3))
          (: h (-> Int Int))
          (define-acc (h x) (+ x 3))]

These demonstrate several ways to ensure that the argument to the function is
an @racket[Int].  The last one makes use of function types, written with
arrows, as described in @secref["arrow-types"].

@subsection[#:tag "importing"]{Importing Racket values}

@defform[(use var type)]

Take a Racket value and make it an Accelerack value.
@; This is done automatically in some cases by @racket[define-acc].

The @emph{var} must be a regular bound variable in Racket, bound to a value
that satisfies @racket[acc-element?] or @racket[acc-array?].  These are the
@emph{only} kinds of data that can be imported from Racket to Accelerack.  Indeed,
most normal Racket datatypes would have no valid Accelerack @emph{type} to
provide to @racket[use]

@; NOTE: Having a problem running this with examples framework
@racketblock[
  (define x (+ 1 2))
  (acc (+ 3 (use x Int)))    

  (define y (acc-array (1 2 3)))
  (define-acc z (map add1 (use y (Array 1 Int))))
  ]
 @; (acc (+ 3 (use (+ 1 2) Int)))  

@; --------------------------------------------------------------
@subsection[#:tag "vector-types"]{Vector (tuple) Types}

Vectors in Accelerack are short, fixed-length data structures that can contain
a mix of different types of data.  (These are called @emph{tuples} in many languages.)

@examples[#:label #f
  (require accelerack)
  (: x #(Int Double Bool))
  (define-acc x (vector 1 2.2 #f))]

@defform[(vector expr_1 ... expr_n)]

Form a vector of of @racket[n] elements.
It's type will be
@racket[ #(type_1 ... type_n) ]
where  @emph{type_1} through @emph{type_n} are the types of
@emph{expr_1} through @emph{expr_n}, respectively.

@; The type of a vector of @racket[n] elements of types @emph{type_1} through @emph{type_n}.

@; defproc[(vector-ref [v vector?] [i exact-nonnegative-integer?]) any]
@defform[(vector-ref v n)]

The type of a @racket[vector-ref] expression is the type of the corresponding
element in the vector.  The index @racket[n] must be a literal number, not an
arbitrary expression.

@; --------------------------------------------------------------
@subsection[#:tag "array-types"]{Array Types}

@defform[(Array n elt)]

The form of an array type is @racket[(Array n elt)] where @racket[n] is a
literal integer specifying the @emph{dimension}, and @racket[elt] is the type
of the array's element.

Arrays, as described earlier, can contain only @racket[acc-element?] values.
Moreover, there is a predicate on types themselves that tells you which types
make valid array elements.

@defproc[(acc-element-type? [t any]) boolean?]
@defproc[(acc-type? [t any]) boolean?]

Both @racket[acc-element-type?] and @racket[acc-type?] are predicates on the
s-expression representation of a type.  They are only for use outside
Accelerack, not within Accelerack expressions.

@examples[
  (require accelerack)
  (acc-type? '(Array 1 Double))
  (acc-element-type? 'Double)
  (acc-element-type? '#(Int Double))
  (acc-element-type? '(Array 1 Double))
  ]

@; -------------------------------------------------------
@subsection[#:tag "arrow-types"]{Function Types}

The type of a function is written @racket[(-> A B)] where @racket[A] is the
input type and @racket[B] is the output type.
@; These function types are sometimes called "arrow" types
We can see this in action in the
types of primitive functions:

@examples[#:label #f (require accelerack)
          (type-of round)
          (type-of quotient)
          (type-of exact->inexact)]

@; -------------------------------------------------------
@subsection[#:tag "type-variables"]{Type Variables and Polymorphism}

What is the type of this function?

@racketblock[  (define-acc (f x) x)]

It can accept @emph{any} value.  We can ask Accelerack for its type as follows:

@examples[#:label #f
  (require accelerack)
  (define-acc (f x) x)
  (type-of f)]

Here the lower-case variables are called @emph{type variables}.  In contrast
with (upper-case) types like @racket[Int] or @racket[Double], they are stand-ins for any
valid Accelerack type.  For example, consider these primitives:

@examples[#:label #f (require accelerack)
  (type-of acc-array-size)
  (type-of acc-array-dimension)]

Because they don't care about the contents of an array, only its size, these
functions are @emph{polymorphic} in array contents and dimension.  That is,
they use type variables to abstract the parts of the type they do not depend on.

Incidentally, note that the above two primitives are part of the subset of
@racket[acc-array] operations that make sense both @emph{inside} and
@emph{outside} of accelerack expressions.  Other array manipulation functions,
like @racket[acc-array->sexp] only make sense in regular Racket code, outside
@racket[acc]/@racket[define-acc] blocks.

@; -------------------------------------------------------
@subsection[#:tag "numeric-types"]{Polymorphic Numeric Types}

What is the type of this function?

@racketblock[  (define-acc (f x) (+ x x))]

We can find out like so:

@examples[#:label #f
  (require accelerack)
  (define-acc (f x) (+ x x))
  (type-of f)]

Here, @racket[f]'s type again uses type variables, but these are special
variables beginning with the prefix @racket[num_].  This is a convention
meaning that the type variable can only be @emph{instantiated} using a numeric
type, i.e. @racket[Int] or @racket[Bool].

Thus all of the primitives which work over @racket[num_] types, such as
@racket[+], @racket[*] and so on, can be used on both integral and floating
point (@racket[Double]) numbers.

@; -------------------------------------------------------
@section[#:tag "debugging"]{Debugging Accelerack}

In addition to responding to type errors as we get them.  It is helpful to be
able to probe the type of expressions to figure out what's going on.  In
addition to the @racket[type-of] construct that we saw above, we can also
enable printing of all inferred types for @racket[define-acc]-bound variabes as
follows:

@defform[(acc-echo-types)]

@examples[#:label #f
  (require accelerack)
  (acc-echo-types)
  (define-acc x (vector #t 3))
  (define-acc a (generate (lambda () 4)))]

Note that if you are following the design recipe, you will want to include
top-level @racket[(: v t)] annotations as the @emph{signature} for each of your
definitions.  However, this echoing capability can help you infer types which
you can then copy-paste to produce your function signatures.

@; -------------------------------------------------------
@section[#:tag "grammar"]{Appendix: Full list of keywords}

If not otherwise mentioned above, the Accelerack syntax is the same as the
normal Racket syntax, e.g. @racket[(if e1 e2 e3)].  However, Accelerack only
recognizes the following subset of Racket syntaxes.

@examples[(require accelerack)
          acc-keywords]
@section[#:tag "grammar"]{Appendix: Full list of primitive types}

@examples[(require accelerack)
          acc-prim-types]
@section[#:tag "grammar"]{Appendix: Full typed-language grammar}

@(require racket/include racket/file)
@(verbatim (file->string "./accelerack_grammar.txt"))

