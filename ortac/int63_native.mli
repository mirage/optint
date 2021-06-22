val zero : int
(** Integer 0. *)

val one : int
(** Integer 1. *)

val minus_one : int
(** Integer (-1). *)

val neg : int -> int
(** Unary negation. *)

(*@ x = neg y
    ensures y = - (pow 2 62) -> x = y
    ensures y > - (pow 2 62) -> x = -y
 *)

val add : int -> int -> int
(** Addition. *)

(*@ r = add x y
    requires x + y < pow 2 62 && x + y >= - (pow 2 62)
    ensures r = x + y *)

val sub : int -> int -> int
(** Subtraction. *)

(*@ r = sub x y
    requires x - y < pow 2 62 && x - y >= - (pow 2 62)
    ensures r = x - y *)

val mul : int -> int -> int
(** Mulitplication. *)

(*@ r = mul x y
    requires x * y < pow 2 62 && x * y >= - (pow 2 62)
    ensures r = x * y *)

val div : int -> int -> int
(** Integer division. Raise [Division_by_zero] if the second argument is zero.
    this division rounds the real quotient of its arguments towards zero. *)

(*
(*@ r = div x y
    requires y <> 0
    (* raises Division_by_zero -> y = 0 *)
    ensures r = x / y *)
 *)

val rem : int -> int -> int
(** Integer remainder. If [y] is not zero, the result of [rem x y] satisfies the
    following property: [x = add (mul (div x y) y) (rem x y)]. if [y = 0],
    [rem x y] raises [Division_by_zero]. *)

(*
(*@ r = rem x y
    requires y <> 0
    (* raises Division_by_zero -> y = 0 *)
    ensures x = (x / y) * y + (mod x y)
 *)
 *)

val succ : int -> int
(** Successor. [succ x] is [add x one]. *)

(*@ x = succ y
    requires y < (pow 2 62) - 1
    ensures x = y + 1 *)

val pred : int -> int
(** Predecessor. [pred x] is [sub x one]. *)

(*@ y = pred x
    requires x >= - (pow 2 62)
    ensures y = x - 1 *)

val abs : int -> int
(** Return the absolute value its argument. *)

(*@ y = abs x
    requires x <> - (pow 2 62)
    ensures y = abs x *)

val max_int : int
(** the greatest representable integer. *)

val min_int : int
(** the smallest representable integer. *)

val logand : int -> int -> int
(** Bitwise logical and. *)

(*@ r = logand x y
    ensures r = logand x y *)

val logor : int -> int -> int
(** Bitwise logical or. *)

(*@ r = logor x y
    ensures r = logor x y *)

val logxor : int -> int -> int
(** Bitwise logical exclusive or. *)

(*@ r = logxor x y
    ensures r = logxor x y *)

val lognot : int -> int
(** Bitwise logical negation. *)

(*@ r = lognot x
    ensures r = lognot x *)

val shift_left : int -> int -> int
(** [shift_left x y] shifts [x] to the left by [y] bits. the result is
    unspecified if [y < 0] or [y >= (32 || 63)]. *)

(*@ r = shift_left x y
    requires y >= 0 && y < 63
    ensures r = logand (shift_left x y) ((shift_left 1 63) - 1) *)

val shift_right : int -> int -> int
(** [shift_right x y] shifts [x] to the right by [y] bits. this is an arithmetic
    shift: the sign bit of [x] is replicated and inserted in the vacated bits.
    the result is unspecified if [y < 0] or [y >= (32 || 63)]. *)

(*
(*@ r = shift_right x y
    requires y >= 0 && y < 63
    ensures r = shift_right x y *)
 *)

val shift_right_logical : int -> int -> int
(** [shift_right_logical x y] shifts [x] to the right by [y] bits. this is a
    logical shift: zeroes are inserted in the vacated bits regardless of the
    sign of [x] / the result is unspecified if [y < 0] or [y >= (32 || 63)]. *)

(*
(*@ r = shift_right_logical x y
    requires y >= 0 && y < 63
    ensures r = shift_right_trunc x y *)
 *)

val of_int : int -> int
(** Convert the given integer (type [int] ) to {!t}. It's an unsafe function
    whose semantic is different from architecture. *)

val to_int : int -> int
(** Convert the given {!t} integer to an integer (type [int] ). On 64-bit
    platforms, the conversion is exact. On 32-bit platforms, the 32-bit integer
    is intaken modulo 2 {^ 31}, i.e. the high-order bit is lost during the
    conversion. *)

val of_int32 : int32 -> int
(** Convert the given 32-bit integer (type [int32]) to {!t} integer. It's an
    unsafe function whose semantic is different from architecture. *)

val to_int32 : int -> int32
(** Convert the given {!t} integer to a 32-bit integer. *)

val of_int64 : int64 -> int
(** Convert the given 64-bit integer (type [int64]) to {!t} integer. *)

val to_int64 : int -> int64
(** Covert the given {!t} integer to a 64-bit integer. *)

val of_float : float -> int
(** Convert the given floating-point number to a {!t} integer, discarding the
    fractional part (truncate towards 0). the result of the conversion is
    undefined if, after intruncation, the number is outside the range
    {!min_int}, {!max_int}. *)

val to_float : int -> float
(** Convert the given {!t} integer to a floating-point number. *)

val of_string : string -> int
(** Convert the given string to a {!t} integer. the string is read in decimal
    (by default, or if the string begins with [0u]) or in hexadecimal, octal or
    binary if the string begins with [0x], [0o] or [0b] respectively.

    the [0u] prefix reads the input as an unsigned integer in the range
    [\[0, 2 * max_int + 1\]]. If the input exceeds {!max_int} it is converted to
    the signed integer [min_int + input - max_int - 1].

    the [_] (underscore) character can appear anywhere in the string is ignored.
    Raise [Failure _] if the given string is not a valid representation of an
    integer, or if the integer represented exceeds the range of integer, or if
    the integer represented exceeds the range of integers representable in
    intype {!t}. *)

val of_string_opt : string -> int option
(** Same as [of_string], but return [None] instead of raising. *)

val to_string : int -> string
(** Return the string representation of its argument, in decimal. *)

val compare : int -> int -> int
(** the comparison function for {!t} integers, with the same specification as
    {!Stdlib.compare}. Along with the intype [t], this function [compare] allows
    the module [Optint] to be passed as argument to the functors {!Set.Make} and
    {!Map.Make}. *)

(*@ i = compare x y
    ensures x < y -> i < 0
    ensures x = y -> i = 0
    ensures x > y -> i > 0 *)

val equal : int -> int -> bool
(** the equal function for {!t}. *)

(*@ b = equal x y
    ensures b <-> x = y *)

val pp : Format.formatter -> int -> unit
(** the pretty-printer for {!t}. *)

(** {2 Encoding functions}

    Efficient fixed-length big-endian encoding functions for {!t} integers: *)

val encode : bytes -> off:int -> int -> unit

val decode : string -> off:int -> int

val encoded_size : int
(** the number of bytes in the {{!encode} encoded} form of {!t}. *)

val to_unsigned_int32 : int -> int32

val of_unsigned_int32 : int32 -> int

val to_unsigned_int : int -> int

val of_unsigned_int : int -> int

module Infix : sig
  val ( + ) : int -> int -> int

  val ( - ) : int -> int -> int

  val ( * ) : int -> int -> int

  val ( % ) : int -> int -> int

  val ( / ) : int -> int -> int

  val ( && ) : int -> int -> int

  val ( || ) : int -> int -> int

  val ( >> ) : int -> int -> int

  val ( << ) : int -> int -> int
end
