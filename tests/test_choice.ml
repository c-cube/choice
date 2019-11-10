open OUnit

module C = Choice

let printer l =  "[" ^ String.concat ";" (List.map string_of_int l) ^ "]"
let print_list_list l = "[" ^ String.concat ";" (List.map printer l) ^ "]"

let test_return () =
  let c = C.mplus (C.return 1) (C.return 2) in
  let l = List.sort compare (C.run_all c) in
  OUnit.assert_equal ~printer [1;2] l
  
let test_delay () =
  let r = ref 0 in
  let generate () =
    incr r;
    C.return !r
  in
  let c = C.delay generate in
  incr r;
  let l = List.sort compare (C.run_n 3 c) in
  OUnit.assert_equal ~printer [2] l

let test_from_fun () =
  let r = ref 0 in
  let generate () =
    incr r;
    Some !r
  in
  let c = C.from_fun generate in
  let l = List.sort compare (C.run_n 5 c) in
  OUnit.assert_equal ~printer [1;2;3;4;5] l

let test_bind () =
  let c1 = C.of_list [1;3] in
  let c2 = C.(c1 >>= fun x -> C.of_list [x; x+1]) in
  let l = List.sort compare (C.run_all c2) in
  OUnit.assert_equal ~printer [1;2;3;4] l

let test_interleave () =
  let c1 = C.of_list [1;3;5] in
  let c2 = C.of_list [2;4;6] in
  let l = ref [] in
  C.iter (C.interleave c1 c2) (fun x -> l := !l @ [x]; true);
  OUnit.assert_equal ~printer [1;2;3;4;5;6] !l

let test_ite1 () =
  let c = C.of_list [1;2] in
  let c' = C.ite c (fun x -> C.return (x+1)) (C.return 42) in
  let l = List.sort compare (C.run_all c') in
  OUnit.assert_equal ~printer [2;3] l

let test_ite2 () =
  let c = C.fail in
  let c' = C.ite c (fun x -> C.return (x+1)) (C.return 42) in
  let l = List.sort compare (C.run_all c') in
  OUnit.assert_equal ~printer [42] l

let test_map () =
  let c = C.of_list [1;2;3;4;5] in
  let c' = C.map succ c in
  let l = List.sort compare (C.run_all c') in
  OUnit.assert_equal ~printer [2;3;4;5;6] l

let test_once () =
  let c = C.of_list [1;2;3] in
  let c' = C.once c in
  let l = List.sort compare (C.run_all c') in
  OUnit.assert_equal ~printer [1] l

let test_guard () = 
  let computation = C.(
      of_list [1;2;3] >>= fun x ->
      guard (x != 2) >>= fun () ->
      return x
    )
  in
  let l = List.sort compare (C.run_all computation) in
  OUnit.assert_equal ~printer [1;3] l

let test_permutations () =
  let l = [1;2;3;4] in
  let g = Gen.permutations (Gen.of_list l) in
  OUnit.assert_equal ~printer:print_list_list
    (Gen.to_rev_list g |> List.sort compare)
    (Choice.List.permutations l |> Choice.Enum.to_list_list |> List.sort compare)

let sort_list l = List.sort compare l

let test_combinations_2 () =
  let l = [1;2;3;4;5] in
  let g = Gen.combinations 2 (Gen.of_list l) in
  OUnit.assert_equal ~printer:print_list_list
    (Gen.to_rev_list g |> List.rev_map sort_list |> sort_list)
    (Choice.List.combinations 2 l |> Choice.Enum.to_list_list
     |> List.rev_map sort_list |> sort_list)

let test_combinations_3 () =
  let l = [1;2;3;4;5] in
  let g = Gen.combinations 3 (Gen.of_list l) in
  OUnit.assert_equal ~printer:print_list_list
    (Gen.to_rev_list g |> List.rev_map sort_list |> sort_list)
    (Choice.List.combinations 3 l |> Choice.Enum.to_list_list
     |> List.rev_map sort_list |> sort_list)

let suite =
  "test_choice" >:::
    [ "test_return" >:: test_return
    ; "test_delay" >:: test_delay
    ; "test_from_fun" >:: test_from_fun
    ; "test_bind" >:: test_bind
    ; "test_interleave" >:: test_interleave
    ; "test_ite1" >:: test_ite1
    ; "test_ite2" >:: test_ite2
    ; "test_map" >:: test_map
    ; "test_once" >:: test_once
    ; "test_guard" >:: test_guard
    ; "test_permutations" >:: test_permutations
    ; "test_combinations_2" >:: test_combinations_2
    ; "test_combinations_3" >:: test_combinations_3
    ]
