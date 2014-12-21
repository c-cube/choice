module C = Choice

let rec insert e l = match l with
  | [] -> C.return [e]
  | x::l' ->
    C.mplus
      (C.return (e :: l))
      C.(insert e l' >>= fun l'' -> return (x :: l''));;

let rec permute = function
  | [] -> C.return []
  | x::l ->
    C.(permute l >>= fun l' -> insert x l')

let rec sorted l = match l with
  | [] | [_] -> true
  | x::((y::l') as l) ->
    x <= y && sorted l;;

let bogosort l =
  C.once (C.filter sorted (permute l));;

let seed = 456

let () =
  let n = int_of_string Sys.argv.(1) in
  let a = CCArray.(n -- 1) in
  let state = Random.State.make [| seed |] in
  CCArray.shuffle_with state a;
  print_endline ("sort array of " ^ string_of_int n ^ " elements");
  let l = C.run_one (bogosort (Array.to_list a)) in
  match l with
  | Some l when sorted l ->
    print_endline "done"
  | _ -> assert false
