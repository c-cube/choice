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

let rec of_list l = match l with
  | [] -> fail
  | x :: l' ->
    { skf=fun sk fk ->
      sk x (fun () -> (of_list l').skf sk fk)
    }

let delay f =
  { skf=(fun sk fk -> (f ()).skf sk fk) }

let bind f x =
  { skf=fun sk fk ->
      x.skf (fun val_x fk -> (f val_x).skf sk fk) fk
  }

let (>>=) x f = bind f x

let rec from_fun f =
  match f () with
  | None -> fail
  | Some x ->
    { skf=(fun sk fk ->
      let fk' () = (from_fun f).skf sk fk in
      sk x fk')
    }

(* reflect operator, the inverse of msplit. It appends the first
 * element (if any) to the remaining ones *)
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
  a.skf
    (fun x fk -> return (Some (x, fk () >>= reflect)))
    (fun () -> return None)

let rec interleave a b =
  msplit a >>= function
  | None -> b
  | Some (val_a, a') ->
    let c = interleave b a' in
    { skf=(fun sk fk ->
      let fk' () = c.skf sk fk in
      sk val_a fk')
    }
  
let rec fair_bind f x =
  msplit x >>= function
  | None -> fail
  | Some (val_x, x') ->
    interleave (f val_x) (fair_bind f x')

let ite c th el =
  msplit c >>= function
  | None -> el
  | Some (val_c, c') ->
    mplus (th val_c) (c' >>= th)

let map f c =
  {skf=(fun sk fk -> c.skf (fun x -> sk (f x)) fk)}

let product a b =
  {skf=(fun sk fk ->
    a.skf
      (fun x fk' ->
        let sk' y fk' = sk (x,y) fk' in
        b.skf sk' fk')
      fk)
  }

let fmap f c = {
  skf=fun sk fk ->
    c.skf (fun x fk -> match f x with
      | Some x -> sk x fk
      | None -> fk()
    ) fk
}

let filter p c = {
  skf=fun sk fk ->
    c.skf
    (fun x fk -> if p x then sk x fk else fk())
    fk
}

let once a = {
  skf=fun sk fk ->
    a.skf (fun x _fk -> sk x fk) fk
}

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
  c.skf (fun x _ -> Some x) (fun () -> None)

let run_n n c =
  let l = ref []
  and n = ref n in
  c.skf
    (fun val_c fk ->
      l := val_c :: !l;
      decr n;
      if !n = 0 then !l else fk ())
    (fun () -> !l)

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

let count c =
  let n = ref 0 in
  c.skf (fun _ fk -> incr n; fk ()) (fun () -> !n)

let run_all c = fold (fun acc x -> x::acc) [] c

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

let (>>-) x f = fair_bind f x
let (++) = mplus
let (<|>) = interleave

let lift f c = {
  skf=fun sk fk ->
    c.skf (fun x fk -> sk (f x) fk) fk
}

let lift2 f a b = {
  skf=fun sk fk ->
    a.skf
      (fun xa fk ->
        b.skf (fun xb fk -> sk (f xa xb) fk) fk
      )
    fk
}

let liftFair f c =
  c >>- fun x -> return (f x)

let liftFair2 f a b =
  a >>- fun x -> b >>- fun y -> return (f x y)

let pure = return

let app f_gen x_gen = {
  skf=fun sk fk ->
    f_gen.skf
      (fun f fk ->
        x_gen.skf
          (fun x fk -> sk (f x) fk)
          fk
      ) fk
}

let ($$) = app

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

  let count e =
    let n = ref 0 in
    let rec count e =
      e.skf
        (fun x fk -> match x with
          | End -> incr n
          | Item (_, e') -> count e'; fk ())
        (fun () -> ())
    in count e; !n

  let to_lists e =
    let rec conv acc e =
      e >>= function
      | End -> return (List.rev acc)
      | Item (x, e') ->
          conv (x::acc) e'
    in conv [] e

    (*
  let to_lists e =
    let rec next acc e =
      e.skf
        (fun item fk1 ->
          match item with
          | End ->
            {
              skf=fun sk fk -> sk (List.rev acc) (fun () -> (fk1()).skf sk fk)
            }
          | Item (x, e_sub) ->
              next (x::acc) e_sub
              ++
              fk1()
        )
        (fun () -> fail)
    in
    next [] e
    *)

  let to_list_list e =
    to_list (to_lists e)
end

module List = struct
  let rec suffixes l = match l with
    | [] -> return []
    | _::l' ->
        { skf=(fun sk fk -> sk l (fun () -> (suffixes l').skf sk fk)); }

  type 'a tree =
    | Empty
    | Leaf of 'a
    | Node of 'a tree * 'a tree

  let rec _tree_of_list = function
    | [] -> Empty
    | x :: l' -> Node (Leaf x, _tree_of_list l')

  let _end = return Enum.End

  (* choose element among [t]. [rest] is elements not to choose from *)
  let rec choose_first rest t = match t with
    | Empty ->
        begin match rest with
        | Empty -> _end
        | Leaf _
        | Node _ -> fail
        end
    | Leaf x -> return (Enum.Item (x, permute_rec rest))
    | Node (l, r) ->
        (choose_first (Node (rest, r)) l)
        ++
        (choose_first (Node (l, rest)) r)
  and permute_rec = function
    | Empty -> return Enum.End
    | Leaf x -> return (Enum.Item (x, _end))
    | Node (l, r) ->
        choose_first l r
        ++
        choose_first r l

  let permutations l =
    let tree = _tree_of_list l in
    permute_rec tree

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

module Array = struct
  (** describes a set of indices to yield *)
  type tree =
    | Empty
    | Leaf of int
    | Node of tree * tree

  let _tree_of_len n =
    let t = ref Empty in
    for i = n-1 downto 0 do
      t := Node (Leaf i, !t)
    done;
    !t

  let _end = return Enum.End

  (* choose element among [t]. [rest] is elements not to choose from *)
  let rec choose_first a rest t = match t with
    | Empty ->
        begin match rest with
        | Empty -> _end
        | Leaf _
        | Node _ -> fail
        end
    | Leaf i -> return (Enum.Item (a.(i), permute_rec a rest))
    | Node (l, r) ->
        (choose_first a (Node (rest, r)) l)
        ++
        (choose_first a (Node (l, rest)) r)
  and permute_rec a = function
    | Empty -> return Enum.End
    | Leaf i -> return (Enum.Item (a.(i), _end))
    | Node (l, r) ->
        choose_first a l r
        ++
        choose_first a r l

  let permutations a =
    let tree = _tree_of_len (Array.length a) in
    permute_rec a tree

  let combinations a = assert false
end
