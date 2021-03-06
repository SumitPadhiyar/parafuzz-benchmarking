module Crowbar = Parafuzz_lib.Crowbar

let rec write f i max = 
    if i <= max then (f i; write f (i+1) max)
    else ()

let test i = function () ->
    let max = 20 in
    let x = Atomic.make 0 in
    let y = Atomic.make 0 in
    let d1 = Domain.spawn(fun () -> write (Atomic.set x) 1 max) in
    let d2 = Domain.spawn(fun () -> write (Atomic.set y) 1 max) in
    Crowbar.check @@ (i <> 100 || (Atomic.get x <> (max-1) || Atomic.get y <> (max-1)));
    Domain.join d1;
    Domain.join d2


let ()  = 
	Crowbar.(add_test ~name:"Basic interleaving test" [Crowbar.int8] (fun i ->
		Parafuzz_lib.run @@ test i
	))
