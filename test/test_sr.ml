open OUnit2

let suite =
  "Service Registry Tests" >:::
    [ Model_test.suite; Db_test.suite; Service_test.suite]   
let () = run_test_tt_main suite
