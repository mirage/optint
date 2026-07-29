let int63 = Alcotest.testable Optint.Int63.pp Optint.Int63.equal

let test00 =
  Alcotest.test_case "Int63.div" `Quick @@ fun () ->
  let open Optint in
  Alcotest.(check int63) "div" (Int63.div Int63.min_int Int63.one) Int63.min_int;
  Alcotest.(check int63) "div" Int63.(div (succ max_int) one) Int63.min_int

let () = Alcotest.run "optint" [ ("int63", [ test00 ]) ]
