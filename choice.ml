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

(** A choice among values of type 'a. It is implemented using a
    success continuation, and a failure continuation
    (SKFT is Success Failure Kontinuation) *)
type 'a t = {
  skf : 'b. ('a, 'b) sk -> 'b fk -> 'b;
}

(** Success continuation *)
and ('a,'b) sk = 'a -> 'b fk -> 'b

(** Failure continuation *)
and 'a fk = unit -> 'a

type 'a choice = 'a t

let return x = { skf = (fun sk fk -> sk x fk) }

let fail = { skf = fun sk fk -> fk () }

let cons x c =
  { skf = fun sk fk -> sk x (fun () -> c.skf sk fk) }

let mplus a b =
  { skf=(fun sk fk ->
    let fk' () = b.skf sk fk in  (* on failure of a, try b *)
    a.skf sk fk')
  }

let of_list l = match l with
  | [] -> fail
  | x::l' ->
    List.fold_left (fun acc x -> mplus acc (return x)) (return x) l'

let delay f =
  { skf=(fun sk fk -> (f ()).skf sk fk) }

let bind x f =
  { skf=
    (fun sk fk ->
      x.skf (fun val_x fk -> (f val_x).skf sk fk) fk)
  }

let (>>=) = bind

let rec from_fun f =
  match f () with
  | None -> fail
  | Some x ->
    { skf=(fun sk fk ->
      let fk' () = (from_fun f).skf sk fk in
      sk x fk')
    }

(* reflect operator, the inverse of msplit *)
let reflect opt = match opt with
  | None -> fail
  | Some (x, c) ->
    { skf=(fun sk fk ->
      let fk' () = c.skf sk fk in
      sk x fk')
    }

(* msplit operator, the base for other combinators. It returns
    the first solution, if any. *)
let msplit (a : 'a t) : ('a * 'a t) option t =
  let fk () = return None in
  let sk x fk = return (Some (x, fk () >>= reflect)) in
  a.skf sk fk

let rec interleave a b =
  msplit a >>= function
  | None -> b
  | Some (val_a, a') ->
    let c = interleave b a' in
    { skf=(fun sk fk ->
      let fk' () = c.skf sk fk in
      sk val_a fk')
    }
  
let rec fair_bind x f =
  msplit x >>= function
  | None -> fail
  | Some (val_x, x') ->
    interleave (f val_x) (fair_bind x' f)

let ite c th el =
  msplit c >>= function
  | None -> el
  | Some (val_c, c') ->
    mplus (th val_c) (c' >>= th)

let map c f =
  {skf=(fun sk fk -> c.skf (fun x -> sk (f x)) fk)}

let product a b =
  {skf=(fun sk fk ->
    a.skf
      (fun x fk' ->
        let sk' y fk' = sk (x,y) fk' in
        b.skf sk' fk')
      fk)
  }

let fmap c f =
  bind c
    (fun x -> match f x with
    | None -> fail
    | Some y -> return y)

let filter c p =
  c >>= fun val_c -> if p val_c then return val_c else fail

let once a =
  msplit a >>= function
  | None -> fail
  | Some (val_c, _) ->
    return val_c

let rec take n c = match n with
  | 0 -> fail
  | 1 -> c
  | _ ->
    assert (n > 0);
    msplit c >>= function
    | None -> fail
    | Some (val_c, c') ->
      mplus (return val_c) (take (n-1) c')

let run_one c =
  let r = ref None in
  try
    c.skf
      (fun val_c _ ->
        r := Some val_c;
        raise Exit)
      (fun () -> ());
    !r
  with Exit ->
    !r

let run_n n c =
  if n = 0
    then []
    else begin
      let l = ref []
      and n = ref n in
      c.skf
        (fun val_c fk ->
          l := val_c :: !l;
          decr n;
          if !n = 0 then () else fk ())
        (fun () -> ());
      !l
    end

let iter c k =
  c.skf
    (fun val_c fk ->
      let continue = k val_c in
      if continue then fk () else ())
    (fun () -> ())

let fold f acc c =
  let acc = ref acc in
  c.skf
    (fun x fk ->
      acc := f !acc x;
      fk ())
    (fun () -> !acc)

let run_all c =
  let l = ref [] in
  c.skf
    (fun val_c fk ->
      l := val_c :: !l;
      fk ())
    (fun () -> ());
  !l

let to_list c = List.rev (run_all c)

let is_empty c =
  c.skf (fun _ _ -> false) (fun () -> true)

let forall c =
  c.skf
    (fun ans fk -> if ans then fk () else false)
    (fun () -> true)

let exists c =
  c.skf
    (fun ans fk -> if ans then true else fk())
    (fun () -> false)

let (>>=) = bind
let (>>-) = fair_bind
let (++) = mplus
let (<|>) = interleave

let lift f c =
  c >>= fun x -> return (f x)

let lift2 f a b =
  a >>= fun x -> b >>= fun y -> return (f x y)

let liftFair f c =
  c >>- fun x -> return (f x)

let liftFair2 f a b =
  a >>- fun x -> b >>- fun y -> return (f x y)

module Enum = struct
  type 'a t = 'a item choice

  and 'a item =
    | End
    | Item of 'a * 'a t

  let next e = e

  let empty = return End

  let cons1 x e = {
    skf=fun sk fk -> sk (Item (x,e)) fk
  }

  let cons head e = {
    skf= fun sk fk ->
      head.skf
        (fun x fk -> sk (Item (x, e)) fk)
        fk
  }

  let rec of_list l = match l with
    | [] -> return End
    | x :: l' ->
        return (Item (x, of_list l'))

  let rec zip a b =
    lift2
      (fun x y -> match x, y with
        | Item (xa, a'), Item (xb, b') ->
            Item ((xa,xb), zip a' b')
        | End, _
        | _, End -> End
      ) a b 

  let to_lists e =
    let rec iter curlist e =
      e >>= function
        | End -> return (List.rev curlist)
        | Item (x, e') -> iter (x::curlist) e'
    in
    fold (fun acc l -> l :: acc) [] (iter [] e)
end

module List = struct
  let rec suffixes l = match l with
    | [] -> return []
    | _::l' ->
        { skf=(fun sk fk -> sk l (fun () -> (suffixes l').skf sk fk)); }

  let permute l =
    (* choose element among [l]. [rest] is elements not to choose from *)
    let rec choose_first l rest = match l with
      | [] -> return Enum.End
      | x::l' ->
        cons
          (Enum.Item (x, permute_rec (List.rev_append rest l')))
          (choose_first l' (x::rest))
    and permute_rec l = match l with
      | [] -> fail
      | _::_ -> choose_first l []
    in
    permute_rec l

  let combinations n l =
    let m = List.length l in
    (* choose [n] elements among the [m] ones of [l] *)
    let rec choose_first n m l = match l with
      | _ when n > m -> fail
      | _ when n=m -> Enum.of_list l
      | [] -> fail
      | x :: l' ->
          cons
            (Enum.Item (x, choose_first (n-1)(m-1) l'))
            (choose_first n (m-1) l')
    in
    choose_first n m l
end
