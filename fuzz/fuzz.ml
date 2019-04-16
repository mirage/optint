type 't t =
  | Val : 't -> 't t
  | Add : 't t * 't t -> 't t
  | Sub : 't t * 't t -> 't t
  | Mul : 't t * 't t -> 't t
  | Div : 't t * 't t -> 't t
  | Rem : 't t * 't t -> 't t
  | Neg : 't t -> 't t
  | Succ : 't t -> 't t
  | Pred : 't t -> 't t
  | Abs : 't t -> 't t
  | Logand : 't t * 't t -> 't t
  | Logor : 't t * 't t -> 't t
  | Logxor : 't t * 't t -> 't t
  | Lognot : 't t -> 't t
  | Shift_left : 't t * int -> 't t
  | Shift_right : 't t * int -> 't t
  | Shift_right_logical : 't t * int -> 't t

module type S = sig
  type t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val abs : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
end

let rec eval
  : type x. (module S with type t = x) -> (x -> 'r) -> x t -> 'r
  = fun (module X) f ->
    let eval = eval (module X) in
    function
    | Val x -> f x
    | Add (a, b) -> eval (fun a -> eval (fun b -> f (X.add a b)) b) a
    | Sub (a, b) -> eval (fun a -> eval (fun b -> f (X.sub a b)) b) a
    | Mul (a, b) -> eval (fun a -> eval (fun b -> f (X.mul a b)) b) a
    | Div (a, b) -> eval (fun a -> eval (fun b -> f (X.div a b)) b) a
    | Rem (a, b) -> eval (fun a -> eval (fun b -> f (X.rem a b)) b) a
    | Logand (a, b) -> eval (fun a -> eval (fun b -> f (X.logand a b)) b) a
    | Logor (a, b) -> eval (fun a -> eval (fun b -> f (X.logor a b)) b) a
    | Logxor (a, b) -> eval (fun a -> eval (fun b -> f (X.logxor a b)) b) a
    | Lognot x -> eval (fun x -> f (X.lognot x)) x
    | Abs x -> eval (fun x -> f (X.abs x)) x
    | Neg x -> eval (fun x -> f (X.neg x)) x
    | Succ x -> eval (fun x -> f (X.succ x)) x
    | Pred x -> eval (fun x -> f (X.pred x)) x
    | Shift_left (x, n) -> eval (fun x -> f (X.shift_left x n)) x
    | Shift_right (x, n) -> eval (fun x -> f (X.shift_right x n)) x
    | Shift_right_logical (x, n) -> eval (fun x -> f (X.shift_right_logical x n)) x

open Crowbar

let binary a b =
  choose [ const (Add (a, b))
         ; const (Sub (a, b))
         ; const (Mul (a, b))
         ; const (Div (a, b))
         ; const (Rem (a, b))
         ; const (Logand (a, b))
         ; const (Logor (a, b))
         ; const (Logxor (a, b)) ]

let unary x =
  choose [ const (Neg x)
         ; const (Succ x)
         ; const (Pred x)
         ; const (Abs x)
         ; const (Lognot x) ]

let shift x n =
  choose [ const (Shift_left (x, n))
         ; const (Shift_right (x, n))
         ; const (Shift_right_logical (x, n)) ]

let ( >>= ) = dynamic_bind
let ( >>| ) x f = map [ x ] f

let gen of_int32 =
  fix @@ fun m ->
  choose [ (m >>= fun a -> m >>= fun b -> binary a b)
         ; (m >>= unary)
         ; (m >>= fun x -> int >>= fun n -> shift x n)
         ; int32 >>| fun x -> Val (of_int32 x) ]

let gen_x86 = gen Optint_x86.Int_x86_backend.of_int32
let gen_x64 = gen Optint_x64.Int_x64_backend.of_int32
let gen_int32 = gen (fun x -> x)

let () =
  add_test ~name:"x86" [ gen_x86; gen_int32 ] @@ fun a b ->
  let a = eval (module Optint_x86.Int_x86_backend) Optint_x86.Int_x86_backend.to_int32 a in
  let b = eval (module Int32) (fun x -> x) b in
  check_eq ~eq:Int32.equal ~pp:Fmt.int32 ~cmp:Int32.compare a b

let () =
  add_test ~name:"x64" [ gen_x64; gen_int32 ] @@ fun a b ->
  let a = eval (module Optint_x64.Int_x64_backend) Optint_x64.Int_x64_backend.to_int32 a in
  let b = eval (module Int32) (fun x -> x) b in
  check_eq ~eq:Int32.equal ~pp:Fmt.int32 ~cmp:Int32.compare a b
