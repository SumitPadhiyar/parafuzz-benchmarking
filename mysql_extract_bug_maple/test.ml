module Crowbar = Parafuzz_lib.Crowbar

module Mysql_log = struct
    open Domain

    type t = { mutable contents: string List.t; mutex: Mutex.t}

    let create () = { contents = []; mutex = Mutex.create ();}

    let write log content = 
        Mutex.lock log.mutex;
        log.contents <- content :: log.contents; 
        Mutex.unlock log.mutex

    let get_tail log = 
        Mutex.lock log.mutex; 
        let v = List.hd log.contents in
        Mutex.unlock log.mutex;
        v

    let reset log = 
        Mutex.lock log.mutex; 
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

let test tab log i = 
    let d1 = Domain.spawn (fun () -> insert tab log) in
    if i = 130 then  
        delete tab log;
    Domain.join d1;

    if Mysql_table.is_empty tab then Crowbar.check @@ (Mysql_log.get_tail log = "remove")
    else Crowbar.check @@ (Mysql_log.get_tail log = "insert")

let ()  =
    let tab = Mysql_table.create () in
    let log = Mysql_log.create () in
	Crowbar.(add_test ~name:"Mysql extract bug maple" [Crowbar.int] (fun i ->
        Parafuzz_lib.run (fun () -> 
            test tab log i;
            Mysql_table.remove_entries tab;
            Mysql_log.reset log
        );
	))
