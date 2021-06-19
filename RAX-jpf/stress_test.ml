module Event = struct
    open Domain

(*     type t = { count: int Atomic.t; mutex: Mutex.t; cv: Condition.t } *)
    type t = { mutex: Mutex.t; cv: Condition.t }

(*     let create () = {count = Atomic.make 0; mutex = Mutex.create (); cv = Condition.create ()} *)
    let create () = { mutex = Mutex.create (); cv = Condition.create ()}

(*     let get_count {count; _} = Atomic.get count *)

    let wait_for_event {mutex; cv} = Mutex.lock mutex; Condition.wait cv mutex; Mutex.unlock mutex

    let signal_event {mutex; cv}= 
(*         Atomic.incr count;  *)
        Mutex.lock mutex; 
        Condition.broadcast cv;
        Mutex.unlock mutex
end

let executive event = function () -> Event.signal_event event

let test i = function () -> 
    let event = Event.create () in
    let d1 = Domain.spawn @@ executive event in
    if(i = 100) then Event.wait_for_event event;
    Domain.join d1

let ()  = 
	Crowbar.(add_test ~name:"Deadlock test" [Crowbar.int] (fun i ->
		test i ()
	))

