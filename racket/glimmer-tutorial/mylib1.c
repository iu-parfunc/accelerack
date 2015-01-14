/**
 *   http://www.cs.grinnell.edu/~rebelsky/Glimmer/Summer2012/RacketFFI/tutorial.html
 */

/**
 * mylib1.c
 *   A collection of functions that I will try to bridge from the
 *   Racket FFI.
 */


// +---------+---------------------------------------------------------
// | Headers |
// +---------+

#include <stdio.h>      // For printf and such.
#include <stdlib.h>     // For malloc and free


// +--------------------+----------------------------------------------
// | Exported Functions |
// +--------------------+

/**
 * Square an integer.
 */
int
isquare (int i)
{
  return i * i;
} // isquare
