module Crowbar = Parafuzz_lib.Crowbar

let test () = 
    let x = Atomic.make 0 in
    let y = Atomic.make 0 in
    let dom = Domain.spawn(fun () -> Crowbar.check @@ not (Atomic.get x = 4)) in
    Atomic.set y 1;
    Atomic.set y 2;
    Atomic.set y 3;
    Atomic.set x 4;
    Domain.join dom

let ()  = 
	Crowbar.(add_test ~name:"Basic interleaving test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
