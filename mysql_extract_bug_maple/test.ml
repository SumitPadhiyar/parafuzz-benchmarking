module Crowbar = Parafuzz_lib.Crowbar

module Mysql_log = struct
    open Domain

    type t = { mutable contents: string List.t; mutable reversed : bool;mutex: Mutex.t}

    let create () = { contents = []; reversed = false; mutex = Mutex.create ();}

    let write log content = 
        Mutex.lock log.mutex;
        log.reversed <- false;
        log.contents <- content :: log.contents; 
        Mutex.unlock log.mutex

    let get_head log = 
        Mutex.lock log.mutex; 
        if not log.reversed then (
            log.contents <- List.rev log.contents;
            log.reversed <- true;
        );
        let v = List.hd log.contents in
        Mutex.unlock log.mutex;
        v

    let reset log = 
        Mutex.lock log.mutex; 
        log.reversed <- false;
        log.contents <- [];
        Mutex.unlock log.mutex; 


end

module Mysql_table = struct
    open Domain

    type t = { mutable contents: (int, int) Hashtbl.t; mutex: Mutex.t}

    let create () = { contents = Hashtbl.create 4; mutex = Mutex.create ();}

    let insert_entry table k v = 
        Mutex.lock table.mutex;
        Hashtbl.add table.contents k v;
        Mutex.unlock table.mutex

    let remove_entries table  = 
        Mutex.lock table.mutex; 
        Hashtbl.reset table.contents;
        Mutex.unlock table.mutex

    let is_empty table = Hashtbl.length table.contents = 0 

end

let delete tab log = 
    Mysql_table.remove_entries tab;
    Mysql_log.write log "remove"

let insert tab log = 
    Mysql_table.insert_entry tab 1 2;
    Mysql_log.write log "insert"

let test tab log = 
    let d1 = Domain.spawn (fun () -> delete tab log) in
    let d2 = Domain.spawn (fun () -> insert tab log) in

    Domain.join d1;
    Domain.join d2;

    if Mysql_table.is_empty tab then Crowbar.check @@ not (Mysql_log.get_head log = "remove")
    else Crowbar.check @@ not (Mysql_log.get_head log = "insert")

let ()  =
    let tab = Mysql_table.create () in
    let log = Mysql_log.create () in
	Crowbar.(add_test ~name:"Mysql extract bug maple" [Crowbar.const 1] (fun _ ->
        Parafuzz_lib.run (fun () -> 
            test tab log;
            Mysql_table.remove_entries tab;
            Mysql_log.reset log
        );
	))
