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
    *)

type 'a t
  (** A choice among values of type 'a *)

(** {2 Combinators} *)

val return : 'a -> 'a t
  (** Return a value, with success *)

val of_list : 'a list -> 'a t
  (** Multiple return *)

val fail : 'a t
  (** Fail to yield a solution *)

val mplus : 'a t -> 'a t -> 'a t
  (** [mplus a b] enumerates choices from [a], then choices from [b]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind. Each solution of the first argument is given to the
      function that returns potentially several choices. *)

val interleave : 'a t -> 'a t -> 'a t
  (** Same as {!mplus}, but fair *)

val fair_bind : 'a t -> ('a -> 'b t) -> 'b t
  (** Fair version of {!bind} *)

val ite : 'a t -> ('a -> 'b t) -> 'b t -> 'b t
  (** [ift c th el] enumerates the choices of [c]. If [c] fails,
      then it behaves like [el], otherwise each solution of [c] is
      given to [th]. *)

val map : 'a t -> ('a -> 'b) -> 'b t
  (** Map solutions *)

val filter : 'a t -> ('a -> bool) -> 'a t
  (** Only keep the solutions that satisfy the given predicate *)

val once : 'a t -> 'a t
  (** Retains at most one solution *)

(** {2 Enumerate solutions} *)

val run_one : 'a t -> 'a option
  (** Run until we get one answer *)

val run_n : int -> 'a t -> 'a list
  (** The [n] first solutions *)

val iter : 'a t -> ('a -> bool) -> unit
  (** Enumerate solutions, until none remains, or the callback returns [false]
      to signal it has had enough solutions *)

(** {2 Infix operators} *)

module Infix : sig
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** Infix version of {! bind} *)

  val (>>-) : 'a t -> ('a -> 'b t) -> 'b t
    (** Infix version of {! fair_bind} *)

  val (++) : 'a t -> 'a t -> 'a t
    (** Infix version of {! mplus} *)

  val (<|>) : 'a t -> 'a t -> 'a t
    (** Infix version of {! interleave} *)
end
