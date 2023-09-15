external of_int32 : int32 -> Int32.t = "%identity"
external of_unsigned_int32 : int32 -> Int32.t = "%identity"
external to_int32 : Int32.t -> int32 = "%identity"
external to_unsigned_int32 : Int32.t -> int32 = "%identity"

let to_int64 = Int64.of_int32
let of_int64 = Int64.to_int32
let pp ppf (x : Int32.t) = Format.fprintf ppf "%ld" x
let without_bit_sign (x : int) = if x >= 0 then x else x land lnot 0x40000000
let invalid_arg fmt = Format.kasprintf invalid_arg fmt

(* XXX(dinosaure): the diff between [to_int] and [to_unsigned_int]
 * is about the sign-bit [0x40000000][int]/[0x80000000][int32].
 *
 * For [to_int], we ensure for a negative number that we use only
 * [0x3fffffff][int32] bits two most significant bits are set to [1].
 * In that case, it safes to cast the [int32] to and [int] (31 bits).
 *
 * For [to_unsigned_int], we don't want to interpret if the value is
 * negative or positive. However, if the number can be interpreted as a
 * negative nnumber, due to the two's complement layout, we are sure
 * to lost, at least, the most significant bit which is a part of unsigned
 * [int32]. So we are able to only accept "positive" numbers.
 *
 * NOTE: we trust on the two's complement! *)

let to_int x =
  if Int32.compare Int32.zero x <= 0 && Int32.compare x Int32.max_int <= 0 then
    Int32.to_int
      x (* XXX(dinosaure): positive and can fit into a 31-bit integer. *)
  else if
    Int32.compare Int32.zero x > 0 && Int32.logand 0xC0000000l x = 0xC0000000l
  then
    let x = Int32.logand x 0x7fffffffl in
    Int32.to_int x
  else invalid_arg "Optint.to_int: %lx can not fit into a 31 bits integer" x

let to_unsigned_int x =
  let max_int = Int32.of_int Stdlib.max_int in
  if Int32.compare Int32.zero x <= 0 && Int32.compare x max_int <= 0 then
    to_int x
  else
    invalid_arg
      "Optint.to_unsigned_int: %lx can not fit into a 31 bits unsigned integer"
      x

let of_unsigned_int x =
  if x < 0 then Int32.logor 0x40000000l (Int32.of_int (without_bit_sign x))
  else Int32.of_int x

let encoded_size = 4

external set_32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32u"
external get_32 : string -> int -> int32 = "%caml_string_get32"
external swap32 : int32 -> int32 = "%bswap_int32"

let encode buf ~off t =
  let t = to_int32 t in
  let t = if not Sys.big_endian then swap32 t else t in
  set_32 buf off t

let decode buf ~off =
  let t = get_32 buf off in
  let t = if not Sys.big_endian then swap32 t else t in
  of_int32 t

module Infix = struct
  let ( + ) a b = Int32.add a b
  let ( - ) a b = Int32.sub a b
  let ( * ) a b = Int32.mul a b
  let ( % ) a b = Int32.rem a b
  let ( / ) a b = Int32.div a b
  let ( land ) a b = Int32.logand a b
  let ( lor ) a b = Int32.logor a b
  let ( lsr ) a b = Int32.shift_right a b
  let ( lsl ) a b = Int32.shift_left a b
  let ( && ) = ( land )
  let ( || ) = ( lor )
  let ( >> ) = ( lsr )
  let ( << ) = ( lsl )
end

include Int32

let of_int x =
  if x < 0 then Int32.logor 0xC0000000l (Int32.of_int (without_bit_sign x))
  else Int32.of_int x
