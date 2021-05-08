module Crowbar = Parafuzz_lib.Crowbar
open Domain

let execute_within_lock lock f = 
        Mutex.lock lock;
        f ();
        Mutex.unlock lock

let items_produced = 2

module Buffer = struct

    let bufsize = 2

    type t = {mutable first : int; mutable last : int; arr : int Array.t; lock: Mutex.t; cv: Condition.t}
    
    let create () = { first = 0; last = 0; arr = Array.make bufsize 0; lock = Mutex.create (); cv = Condition.create ()}

    let enq buf x =
        while ((buf.last+1) mod bufsize) = buf.first do
            execute_within_lock buf.lock (fun () -> Condition.wait buf.cv buf.lock);
            Crowbar.check (((buf.last+1) mod bufsize) <> buf.first)
        done;
        buf.arr.(buf.last) <- x;
        buf.last <- (buf.last + 1) mod bufsize;
        execute_within_lock buf.lock (fun () -> Condition.signal buf.cv)

    let deq buf =
        while buf.first = buf.last do
            execute_within_lock buf.lock (fun () -> Condition.wait buf.cv buf.lock)
        done;
        let v = buf.arr.(buf.first) in
        buf.first <- (buf.first + 1) mod bufsize;
        execute_within_lock buf.lock (fun () -> Condition.signal buf.cv);
        v

end

    
let producer buf name = 
    let rec loop i = 
        if i < items_produced then (
            Buffer.enq buf name;
            loop (i+1)
        ) else ()
    in
    loop 0


let consumer buf =
    let rec loop i = 
        if i < (items_produced * 2) then (
            ignore @@ Buffer.deq buf;
            loop (i+1)
        ) else ()
    in
    loop 0


let test () = 
    let buf = Buffer.create () in
    let p1 = Domain.spawn (fun () -> producer buf 1) in
    let p2 = Domain.spawn (fun () -> producer buf 2) in
    let p3 = Domain.spawn (fun () -> consumer buf) in
    Domain.join p1;
    Domain.join p2;
    Domain.join p3

let ()  = 
	Crowbar.(add_test ~name:"BufferNotify test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run test
	))

