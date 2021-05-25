module Crowbar = Parafuzz_lib.Crowbar
open Domain

let x = Atomic.make 0
let max_run_per_domain = 10 
let schedule = ref ""

let rec run_d1 i = 
    if i <= max_run_per_domain then (
        Atomic.set x 0;
        schedule := !schedule ^ (string_of_int @@ Scheduler.get_current_domain_id ());
        match !schedule with
        | "112312133232123221323211" -> Crowbar.check false
        | _ ->
        run_d1 (i+1)
    ) else ()
        
let rec run_d2 i = 
    if i <= max_run_per_domain then (
        Atomic.set x 0;
        schedule := !schedule ^ (string_of_int @@ Scheduler.get_current_domain_id ());
        run_d2 (i+1)
    ) else ()

let rec run_d3 i = 
    if i <= max_run_per_domain then (
        Atomic.set x 0;
        schedule := !schedule ^ (string_of_int @@ Scheduler.get_current_domain_id ());
        run_d3 (i+1)
    ) else ()

let test () = 
  schedule := "";
  let d1 = spawn (fun () -> run_d1 1) in
  let d2 = spawn (fun () -> run_d2 1) in
  let d3 = spawn (fun () -> run_d3 1) in
  join d1;
  join d2;
  join d3


let ()  = 
  Crowbar.(add_test ~name:"Large Schedule-space Benchmark" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))
  
