open OUnit
open Warmup

let suite = "OUnit tests..."
            >:::  ["Fix me"
                   >:: (fun () -> assert_equal (insertion_sort [4;5;1;3;6;2]) [1;2;3;4;5;6])]

let _ = run_test_tt_main suite
