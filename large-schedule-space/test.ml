module Crowbar = Parafuzz_lib.Crowbar
open Domain

let func mutex acc v = 
  let _ = Mutex.lock mutex in
  acc := !acc ^ (string_of_int v);
  let _ = Mutex.unlock mutex in ()

(* Generates a string of increasing numbers till n *)
let rec makestr n i acc = 
  if i = n then acc
  else makestr n (i+1) (acc ^ (string_of_int i))

let test () = 
  let str = ref "" in
  let mutex = Mutex.create () in
  let res = makestr 10 0 "" in
  let domains = List.init 10 (fun i -> Domain.spawn (fun () -> func mutex str i)) in
  List.iter (Domain.join) domains;
  Crowbar.check(not (String.equal res (!str)))

let ()  = 
  Crowbar.(add_test ~name:"Large Schedule-space Benchmark" [Crowbar.const 1] (fun _ ->
  Parafuzz_lib.run (test)
  ))
  