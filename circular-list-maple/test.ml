module Crowbar = Parafuzz_lib.Crowbar
open Domain

type node = {
    mutable data : int;
    mutable next : node option
}

type t = {
    mutable head : node option;
    mutable tail : node option;
    mutable size : int;
    lock : Mutex.t
}

let create () = {head = None; tail = None; size = 0; lock = Mutex.create ()} 

let list_push_back cl i = 
    let new_node = {data = i; next = None} in
    Mutex.lock cl.lock;
    (match cl.tail with
    | None -> 
            cl.head <- Some new_node;
            cl.tail <- Some new_node
    | Some tail -> 
            tail.next <- Some new_node;
            cl.tail <- Some new_node
    );
    cl.size <- cl.size + 1;
    Mutex.unlock cl.lock

let list_pop_front cl = 
    let ret = ref None in
    let node = ref None in
    Mutex.lock cl.lock;
    (match cl.head with
        | None -> ()
        | Some tail ->
            ret := Some ((Option.get cl.head).data);
            node := cl.head;
            if Option.is_some cl.head && Option.is_some cl.tail && Option.equal (fun a b -> a == b) cl.head cl.tail then (
                cl.head <- (Option.get cl.head).next;
                cl.tail <- cl.head
            ) else cl.head <- (Option.get cl.head).next
    ); 
    cl.size <- cl.size - 1;
    Mutex.unlock cl.lock;
    !ret

let process cl =
    let value = list_pop_front cl in
    if Option.is_some value then list_push_back cl (Option.get value + 10)

let test i = function () ->
    let cl = create () in
    let rec add_elem i = 
        match i < 10 with
        | true -> list_push_back cl (i); add_elem (i+1)
        | false -> ()
    in
    add_elem 0;
    let d1 = Domain.spawn (fun () -> process cl) in
    let d2 = Domain.spawn (fun () ->  if i = 130 then process cl ) in
    Domain.join d1;
    Domain.join d2;
    
    let rec iterate cl prev_num = 
        match cl with
        | Some node -> 
                if prev_num <> -1 then Crowbar.check (node.data > prev_num);
                iterate node.next node.data
        | None -> ()
    in
    iterate (cl.head) (-1)

let ()  = 
	Crowbar.(add_test ~name:"Circular list test" [Crowbar.int] (fun i ->
		Parafuzz_lib.run @@ test i
	))
