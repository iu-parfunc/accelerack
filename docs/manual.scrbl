#lang scribble/manual

@(require (for-label (except-in racket map)
                     accelerack
                     2htdp/image))

@title{Data-Parallel Programming with Accelerack}

In this course we are using a library called @racket[accelerack],
which allows you to use Racket to do data-parallel programming
for GPUs.

Start by requiring the @racket[accelerack] module:

@defmodule[accelerack]

Once you have done that, some of the functions you are familiar with
(such as @racket[map]) have been replaced with expanded versions. 


@section[#:tag "arrays"]{Arrays}

@defproc[(acc-array [acc-element acc-element?] ...) acc-array?]

Arrays are collections of values. Unlike lists, they are fixed in
size, and can be indexed efficiently.

@racketblock[
(define-acc x (acc-array (1 2 3)))
(check-true (acc-array? x))
]

@defproc[(acc-array? [array any]) boolean?]

Determine whether a value is an @racket[acc-array].

@defproc[(acc-element? [element any]) boolean?]

Determine whether a value can go inside an @racket[acc-array], that is, whether it can be an
array @emph{element}.  This currently includes: exact integers, inexact floating point numbers, and booleans.
Also, vectors of elements are also valid elements (including vectors of vectors).

@defproc[(acc-array->sexp [acc-array acc-array?]) sexp?]

Arrays can be converted into ordinary Racket s-expressions.
The result is a nested series of lists, with nesting depth equal to the dimension of the array.
The element values will all satisfy @racket{acc-element?}.

Note that the s-expression uses the same format as the @racket{acc-array} syntax for constructing literal arrays.

@racketblock[
(define x (acc-array 15))
(acc-array->sexp x)
]

@defproc[(acc-array-ref [acc-array acc-array?] 
                        [index integer?] ...) acc-element?]

To reference an element in an array, you provide its index. The
@racket[acc-array-ref] function takes as many index parameters
as there are dimensions in the input array.

@defproc[(acc-array-flatref [acc-array acc-array?]
                            [flat-index integer?]) acc-element?]

Like @racket[acc-array-ref], this function gets an element out
of an array using an index. Unlike @racket[acc-array-ref], this
function only takes one integer as the index. This integer is the
row-major order index of the element. 

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

@defproc[(acc-array-size [acc-array acc-array?])
         integer?]

Returns the size of an array.

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
@section[#:tag "functions"]{Basic Functions}

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


