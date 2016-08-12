(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Backtracking monad} *)

(** This is an attempt to implement the Logic monad, as described
    in {{: http://hackage.haskell.org/packages/archive/logict/0.2.3/doc/html/Control-Monad-Logic.html}
    this Haskell library}
    or in {{: http://www.cs.rutgers.edu/~ccshan/logicprog/LogicT-icfp2005.pdf} this paper}.

    Some design choices are slightly different, since OCaml is strict.
    Performance may also vary from ghc-compiled code, since it performs some
    optimizations like deforestation.

    {3 Example}

    We adapt the example from the LogicT paper.

{[let rec insert e l = match l with
  | [] -> return [e]
  | x::l' ->
    mplus
      (return (e :: l))
      (insert e l' >>= fun l'' -> return (x :: l''));;

let rec permute = function
  | [] -> return []
  | x::l ->
    permute l >>= fun l' ->
    insert x l';;

let rec sorted l = match l with
  | [] | [_] -> true
  | x::((y::l') as l) ->
    x <= y && sorted l;;

let bogosort l =
  once (filter sorted (permute l));;
  ]}

  Then, running
  {[run_n 1 (bogosort [2;3;5;1;0]);; ]}
  yields 
  {[- : int list list = [[0; 1; 2; 3; 5]] ]}
    *)

type 'a t
  (** A choice among values of type 'a *)

type 'a choice = 'a t

(** {2 Combinators} *)

val return : 'a -> 'a t
  (** Return a value, and succeed *)

val of_list : 'a list -> 'a t
  (** Multiple returns. Each element of the list is a candidate
      for success. *)

val from_fun : (unit -> 'a option) -> 'a t
  (** Call the function to get alternative choices.
      Example:
      {[let r = ref 0 in Choice.run_n 10
        (Choice.filter
          (Choice.from_fun (fun () -> incr r; Some !r)) (fun x -> x mod 3 = 0));;]}
      yields
      {[- : int list = [30; 27; 24; 21; 18; 15; 12; 9; 6; 3] ]} *)

val delay : (unit -> 'a t) -> 'a t
  (** Delay the computation (the closure will be called in each branch
      that uses the choice point *)

val fail : 'a t
  (** Fail to yield a solution. *)

val cons : 'a -> 'a t -> 'a t
  (** [cons x c] is a shortcut for [return x ++ c] *)

val mplus : 'a t -> 'a t -> 'a t
  (** [mplus a b] enumerates choices from [a], then choices from [b]. *)

val bind : ('a -> 'b t) -> 'a t -> 'b t
  (** Monadic bind. Each solution of the first argument is given to the
      function, that may in turn return several choices. *)

val interleave : 'a t -> 'a t -> 'a t
  (** Same as {!mplus}, but fair, ie it enumerates solutions
      alternatively from its first and second arguments. *)

val fair_bind : ('a -> 'b t) -> 'a t -> 'b t
  (** Fair version of {!bind}. *)

val ite : 'a t -> ('a -> 'b t) -> 'b t -> 'b t
  (** [ite cond th el] enumerates the choices of [cond]. If [cond] fails,
      then it behaves like [el], otherwise each solution of [cond] is
      given to [th]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map solutions to other solutions *)

val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product of two choices *)

val fmap : ('a -> 'b option) -> 'a t -> 'b t
  (** Special case of {! bind}, with only zero or one possible
      output choices for each input choice. *)

val filter : ('a -> bool) -> 'a t -> 'a t
  (** Only keep the solutions that satisfy the given predicate. *)

val once : 'a t -> 'a t
  (** Retain at most one solution (drop alternatives). *)

val take : int -> 'a t -> 'a t
  (** Retain at most [n] solutions *)

(** {2 Enumerate solutions} *)

val run_one : 'a t -> 'a option
  (** Run until we get one answer (or a failure) *)

val run_n : int -> 'a t -> 'a list
  (** The [n] first solutions, in {b reverse} order. *)

val run_all : 'a t -> 'a list
  (** All the solutions (in reverse order) *)

val to_list : 'a t -> 'a list
  (** All the solutions (in correct order) *)

val iter : 'a t -> ('a -> bool) -> unit
  (** Enumerate solutions, until none remains, or the callback returns [false]
      to signal it has had enough solutions *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Fold over all solutions *)

val count : _ t -> int
  (** Number of solutions *)

val is_empty : _ t -> bool
  (** return [true] iff the alternative stream is empty (failure) *)

val forall : bool t -> bool

val exists : bool t -> bool

(** {2 Monadic operators} *)

val lift : ('a -> 'b) -> 'a t -> 'b t

val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val liftFair : ('a -> 'b) -> 'a t -> 'b t

val liftFair2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val pure : ('a -> 'b) -> ('a -> 'b) t

val app : ('a -> 'b) t -> 'a t -> 'b t
  (** Applicative instance *)

val ($$) : ('a -> 'b) t -> 'a t -> 'b t
  (** Shortcut for {!app} *)

(** {2 Infix operators} *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix version of {! bind} *)

val (>>-) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix version of {! fair_bind} *)

val (++) : 'a t -> 'a t -> 'a t
  (** Infix version of {! mplus} *)

val (<|>) : 'a t -> 'a t -> 'a t
  (** Infix version of {! interleave} *)

(** {2 Enumerator} *)

module Enum : sig
  type 'a t
    (** Enumerate values of type 'a, with a choice point for each value *)

  type 'a item =
    | End
    | Item of 'a * 'a t

  val next : 'a t -> 'a item choice
    (** choose the next element *)

  val empty : 'a t
    (** Empty iterator *)

  val cons : 'a choice -> 'a t -> 'a t
    (** Add an element at the head of the list *)

  val cons1 : 'a -> 'a t -> 'a t
    (** deterministic cons *)

  val of_list : 'a list -> 'a t
    (** Simply enumerate the elements of the list *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
    (** Combine every element of the first enumeration with every
        element at the same position in the second enumeration.
        The result always ends at soon as one of the enumeration ends *)

  val count : _ t -> int
    (** Number of distincts enumerations *)

  val to_lists : 'a t -> 'a list choice
    (** Conversion to a choice of lists *)

  val to_list_list : 'a t -> 'a list list
    (** Conversion to a list of possibilies *)
end

(** {2 More complex Combinators} *)

module List : sig
  val suffixes : 'a list -> 'a list t
    (** Suffixes of the list *)

  val permutations : 'a list -> 'a Enum.t
    (** Enumerate the items of the list in any order *)

  val combinations : int -> 'a list -> 'a Enum.t
    (** Combinations of [n] elements of the list *)
end

module Array : sig
  val permutations : 'a array -> 'a Enum.t

  val combinations : int -> 'a array -> 'a Enum.t
end
