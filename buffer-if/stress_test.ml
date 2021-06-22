open Domain

let items_produced = 10

module Buffer = struct

    let bufsize = (items_produced * 2 ) - 1

    type t = {mutable first : int; mutable last : int; arr : int Array.t; lock: Mutex.t; cv: Condition.t}
    
    let create () = { first = 0; last = 0; arr = Array.make bufsize 0; lock = Mutex.create (); cv = Condition.create ()}

    let enq buf x =
        Mutex.lock buf.lock;
        if ((buf.last+1) mod bufsize) = buf.first then ( 
            Condition.wait buf.cv buf.lock;
        );
        buf.arr.(buf.last) <- x;
        buf.last <- (buf.last + 1) mod bufsize;
        Condition.broadcast buf.cv;
        Mutex.unlock buf.lock

    let deq buf =
        Mutex.lock buf.lock;
        while buf.first = buf.last do
            Condition.wait buf.cv buf.lock;
        done;
        let v = buf.arr.(buf.first) in
        buf.first <- (buf.first + 1) mod bufsize;
        Condition.broadcast buf.cv;
        Mutex.unlock buf.lock;
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
	Crowbar.(add_test ~name:"BufferIf test" [Crowbar.const 1] (fun _ ->
		test ()
	))

