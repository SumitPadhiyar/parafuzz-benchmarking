module Crowbar = Parafuzz_lib.Crowbar
open Domain

let a = Domain.Mutex.create ()
let b = Domain.Mutex.create ()
let c = Domain.Mutex.create ()

let run order = 
    if order = 12 then (
        Mutex.lock a;
        Mutex.lock b;
        Mutex.unlock b;
        Mutex.unlock a;
    ) else if order = 24 then (
        Mutex.lock b;
        Mutex.lock c;
        Mutex.unlock c;
        Mutex.unlock b;
    ) else if order = 36 then (
        Mutex.lock c;
        Mutex.lock a;
        Mutex.unlock a;
        Mutex.unlock c;
    )

let test i j k = function () ->
    let d1 = Domain.spawn (fun () -> run i ) in
    let d2 = Domain.spawn (fun () -> run j ) in
    let d3 = Domain.spawn (fun () -> run k ) in
    Domain.join d1;
    Domain.join d2;
    Domain.join d3

let ()  = 
    let max = 1000 in
    Crowbar.(add_test ~name:"Deadlock3 test" [Crowbar.range max; Crowbar.range max; Crowbar.range max ] (fun i j k ->
		Parafuzz_lib.run @@ test i j k 
	))
