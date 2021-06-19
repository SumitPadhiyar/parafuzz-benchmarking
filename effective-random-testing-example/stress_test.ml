
let rec write f i max = 
    if i <= max then (f i; write f (i+1) max)
    else ()

let test () = 
    let max = 20 in
    let x = Atomic.make 0 in
    let y = Atomic.make 0 in
    let d1 = Domain.spawn(fun () -> write (Atomic.set x) 1 max) in
    let d2 = Domain.spawn(fun () -> write (Atomic.set y) 1 max) in
    Crowbar.check @@ (Atomic.get x <> (max-1) || Atomic.get y <> (max-1));
    Domain.join d1;
    Domain.join d2


let ()  = 
	Crowbar.(add_test ~name:"Basic interleaving test" [Crowbar.const 1] (fun _ ->
		test ()
	))
