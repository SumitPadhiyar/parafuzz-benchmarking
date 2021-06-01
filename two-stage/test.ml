module Crowbar = Parafuzz_lib.Crowbar

open Domain

let setv v mutex = 
  Mutex.lock mutex;
  v := 1;
  Mutex.unlock mutex
;;

let setw v w mutex = 
  Mutex.lock mutex;
  w := !v + 1;
  Mutex.unlock mutex
;;

let read_write v w mutex = 
  let t1 = ref (-1) in 
  let t2 = ref (-1) in
  Mutex.lock mutex;
  t1 := !v;
  Mutex.unlock mutex;
  Mutex.lock mutex;
  t2 := !w;
  Mutex.unlock mutex;
  Crowbar.check (!t2 != !t1 + 1)
;;

let test () = 
  let v = ref 0 in
  let w = ref 0 in
  let m = Mutex.create () in
  let d1 = Domain.spawn (fun () -> setv v m) in
  let d2 = Domain.spawn (fun () -> setw v w m) in
  let d3 = Domain.spawn (fun () -> read_write v w m) in
  Domain.join d3;
  Domain.join d2;
  Domain.join d1

let ()  =
  Crowbar.(add_test ~name:"Two stage benchmark" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))

