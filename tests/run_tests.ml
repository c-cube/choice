
open OUnit

let suite =
  "all_tests" >:::
    [ Test_choice.suite
    ]

let _ =
  run_test_tt_main suite

