module Crowbar = Parafuzz_lib.Crowbar

let test i () = 
    let x = Atomic.make i in
    let y = Atomic.make 0 in
    let dom = Domain.spawn(fun () -> if (Atomic.get x = 10) then Atomic.set y 2) in
    Atomic.set y 1;
    Domain.join dom;
    Crowbar.check @@ not (Atomic.get y = 2)

let ()  = 
	Crowbar.(add_test ~name:"Interleaving + input test" [Crowbar.int] (fun i ->
		Parafuzz_lib.run @@ test i
	))
