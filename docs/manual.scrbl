#lang scribble/manual

@(require (for-label (except-in racket map)
                     accelerack
                     2htdp/image))

@title{Data-Parallel Programming with Accelerack (v0.1)}

In this course we are using a library called @racket[accelerack],
which allows you to use Racket to do data-parallel programming
for GPUs.

Start by requiring the @racket[accelerack] module:

@defmodule[accelerack]

Once you have done that, some of the functions you are familiar with
(such as @racket[map]) have been replaced with expanded versions. 



@section[#:tag "arrays"]{Array datatype and properties}

@defproc[(acc-array [acc-element acc-element?] ...) acc-array?]

Arrays are collections of values. Unlike lists, they are fixed in
size, and can be indexed efficiently.

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

@section[#:tag "arrays"]{Array elements and element-wise access}

@defproc[(acc-element? [element any]) boolean?]

Determine whether a value can go inside an @racket[acc-array], that is, whether it can be an
array @emph{element}.  This currently includes: exact integers, inexact floating point numbers, and booleans.
Also, vectors of elements are also valid elements (including vectors of vectors).

@defproc[(acc-array-ref [acc-array acc-array?] 
                        [index integer?] ...) acc-element?]

To reference an element in an array, you provide its index. The
@racket[acc-array-ref] function takes as many index parameters
as there are dimensions in the input array.

For example, for a 3D array @racket[arr] with shape @racket[(vector x y z)], the
smallest and largest valid indices are
@racket[(acc-array-ref arr 0 0 0)] and
@racket[(acc-array-ref arr (sub1 x) (sub1 y) (sub1 z))].
These are diagonally opposite "corners" of the volume.

@defproc[(acc-array-flatref [acc-array acc-array?]
                            [flat-index integer?]) acc-element?]

Like @racket[acc-array-ref], this function gets an element out
of an array using an index. Unlike @racket[acc-array-ref], this
function only takes one integer as the index. This integer is the
row-major order index of the element. 
Valid indices are thus between 0 and @racket[acc-array-size] minus 1.


@section[#:tag "arrays"]{Array conversions}

@defproc[(acc-array->sexp [acc-array acc-array?]) sexp?]

Arrays can be converted into ordinary Racket s-expressions.
The result is a nested series of lists, with nesting depth equal to the dimension of the array.
The element values will all satisfy @racket{acc-element?}.

Note that the s-expression uses the same format as the @racket{acc-array} syntax for constructing literal arrays.

@racketblock[
(define x (acc-array 15))
(acc-array->sexp x)
]


@;{
@; -------------------------------------------------------
@section[#:tag "computations"]{Accelerack Computations}

@defform*[((define-acc (name args ...) body)
           (define-acc name expr))]

Define an Accelerack computation, either a function or expression.

@defproc[(use [any any?]) acc-array?]

Take a Racket value and make it an Accelerack value. This is done
automatically in some cases by @racket[define-acc].
}

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
