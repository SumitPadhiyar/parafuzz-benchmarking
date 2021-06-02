module Crowbar = Parafuzz_lib.Crowbar

open Domain

module OnDemand = struct
  type t = {
    mutable critical:int;
    mutable lock:Mutex.t
  }

  let create () = {
    critical = 0;
    lock = Mutex.create ()
  }

  let modify onDemand = fun () ->
    onDemand.lock <- Mutex.create ();
    Mutex.lock (onDemand.lock);
    onDemand.critical <- (onDemand.critical) + 1;
    Mutex.unlock (onDemand.lock)

end

let test () = 
  let od = OnDemand.create () in 
  let threads = List.init 5 (fun _ -> Domain.spawn (OnDemand.modify od)) in
  List.iter (Domain.join) threads;
  Printf.printf "%d\n" od.critical;
  Crowbar.check (od.critical = 5)

let ()  = 
  Crowbar.(add_test ~name:"On Demand Locking Benchmark" [Crowbar.const 1] (fun _ ->
  Parafuzz_lib.run test
  ))
  
