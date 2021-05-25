module Crowbar = Parafuzz_lib.Crowbar
open Domain

let x = Atomic.make 0

let schedule_max_length = 10 

let rec makestr_rev start acc = 
  if start < 1 then acc
  else makestr_rev (start-1) (acc ^ (string_of_int start))

let rec makestr max start acc = 
    if start <= max then (
      makestr max (start+1) (acc ^ (string_of_int start)) 
    ) else acc

let generate_schedule_string count = 
    let divide_point = count/2 in
    let res = makestr_rev divide_point "" in
    res ^ (makestr count (divide_point+1) "")

let res = generate_schedule_string schedule_max_length 
let str = ref ""

(* let check_for_buggy_schedule acc =   *)
(*     Crowbar.check (res <> !acc) *)

let func acc v = 
     ignore @@ Atomic.get x;
     acc := !acc ^ (string_of_int v)
(*      check_for_buggy_schedule acc   *)

let test () = 
  str := "";
  let domains = List.init schedule_max_length (fun i -> Domain.spawn (fun () -> func str (i+1))) in
  List.iter (Domain.join) domains;
  Crowbar.check (res <> !str)

let ()  = 
  Crowbar.(add_test ~name:"Large Schedule-space Benchmark" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))
  
