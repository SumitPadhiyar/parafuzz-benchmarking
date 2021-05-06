module Crowbar = Parafuzz_lib.Crowbar
open Domain

let a = Domain.Mutex.create ()
let b = Domain.Mutex.create ()
let c = Domain.Mutex.create ()

let run order = 
    if order = 0 then (
        Mutex.lock a;
        Mutex.lock b;
        Mutex.unlock b;
        Mutex.unlock a;
    )
    else if order = 1 then (
        Mutex.lock b;
        Mutex.lock c;
        Mutex.unlock c;
        Mutex.unlock b;
    )
    else (
        Mutex.lock c;
        Mutex.lock a;
        Mutex.unlock a;
        Mutex.unlock c;
    )

let test () = 
    let d1 = Domain.spawn (fun () -> run 0 ) in
    let d2 = Domain.spawn (fun () -> run 1 ) in
    let d3 = Domain.spawn (fun () -> run 2 ) in
    Domain.join d1;
    Domain.join d2;
    Domain.join d3

let ()  = 
	Crowbar.(add_test ~name:"Deadlock3 test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
