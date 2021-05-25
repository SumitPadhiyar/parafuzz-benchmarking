module Crowbar = Parafuzz_lib.Crowbar

let rec write f i max = 
    if i <= max then (f i; write f (i+1) max)
    else ()

let test () = 
    let max = 100 in
    let x = Atomic.make 0 in
    let y = Atomic.make 0 in
    let d1 = Domain.spawn(fun () -> write (Atomic.set x) 1 max) in
    let d2 = Domain.spawn(fun () -> write (Atomic.set y) 1 max) in
    let xval = Atomic.get x in
    let yval = Atomic.get y in
(*     Printf.printf "%d %d\n" (xval) (yval); *)
(*     Crowbar.check @@ (Atomic.get x <> (max/2) || Atomic.get y <> (max/2) || Atomic.get z <> (max/2)); *)
    Crowbar.check @@ (xval <> (max/2) || yval <> (max/2));
    Domain.join d1;
    Domain.join d2

let ()  = 
	Crowbar.(add_test ~name:"Large schedule space test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))
