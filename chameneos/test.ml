module List = ListLabels
module String = StringLabels
module Crowbar = Parafuzz_lib.Crowbar

open Printf
open Domain

module Color = struct
  type t =
  | Blue
  | Red
  | Yellow

  let complement t t' =
  match t, t' with
    | Blue, Blue -> Blue
    | Blue, Red -> Yellow
    | Blue, Yellow -> Red
    | Red, Blue -> Yellow
    | Red, Red -> Red
    | Red, Yellow -> Blue
    | Yellow, Blue -> Red
    | Yellow, Red -> Blue
    | Yellow, Yellow -> Yellow

end

module Meeting_place = struct

  type 'chameneos t = {
    mutable state : [ `Empty | `First of 'chameneos | `Second of 'chameneos ];
    mutable meetings_left : int;
    mutex : Mutex.t;
    wait_for_second : Condition.t;
    wait_for_empty : Condition.t;
  }

  let create n = {
    state = `Empty;
    meetings_left = n;
    mutex = Mutex.create ();
    wait_for_second = Condition.create ();
    wait_for_empty = Condition.create ();
  }

  let meet t c =
    let rec loop () =
      if t.meetings_left = 0 then begin
        Condition.broadcast t.wait_for_empty;
        None
      end
      else
  match t.state with
  | `Empty ->
      t.state <- `First c;
      Condition.wait t.wait_for_second t.mutex;
      begin
        match t.state with
        | `Empty
        | `First _ ->
      assert false
        | `Second c ->
      t.state <- `Empty;
                  Condition.signal t.wait_for_empty;
                  Condition.signal t.wait_for_empty;
      Some c
      end
  | `First c1 ->
      t.state <- `Second c;
      t.meetings_left <- t.meetings_left - 1;
      Condition.signal t.wait_for_second;
      Some c1
  | `Second _ ->
      Condition.wait t.wait_for_empty t.mutex;
      loop ()
    in
    Mutex.lock t.mutex;
    let res = loop () in
    Mutex.unlock t.mutex;
    res
  ;;
end

module Chameneos = struct

  type t = {
    id : int;
    mutable color : Color.t;
    mutable meetings : int;
    mutable meetings_with_self : int;
  }

  let create =
    let id = ref 0 in
    let new_id () =
      let r = !id in
      id := r + 1;
      r
    in
    fun color ->
      { id = new_id ();
  color = color;
  meetings = 0;
  meetings_with_self = 0;
      }

  let run t place =
    let rec loop () =
      match Meeting_place.meet place t with
      | None -> ()
      | Some other ->
    t.meetings <- t.meetings + 1;
    if t.id = other.id then t.meetings_with_self <- t.meetings_with_self + 1;
    t.color <- Color.complement t.color other.color;
    loop ()
    in
    Domain.spawn loop
end

let check_colors ch_list =
  List.fold_left ~f:(fun acc c -> if c.Chameneos.color = Color.Yellow then acc+1 else acc) ~init:0 ch_list

let count cs = 
  List.fold_left ~f:(fun (b,r,y) c -> 
    match c.Chameneos.color with
    | Color.Blue -> (b+1, r, y)
    | Color.Red -> (b, r+1, y)
    | Color.Yellow -> (b, r, y+1)
    ) ~init:(0,0,0) cs

let check (b, r, y) = 
  match (b, r, y) with
  | (0, 0, _) -> false
  | (0, _, 0) -> false
  | (_, 0, 0) -> false
  | _ -> true
  

let work chameneos_list num_meets =
  let module C = Chameneos in
  let place = Meeting_place.create num_meets in
  let cs = List.map chameneos_list ~f:Chameneos.create in
  let threads = List.map cs ~f:(fun c -> Chameneos.run c place) in
  let (b, r, y) = count cs in
  Crowbar.check (check (b, r, y));
  List.iter Domain.join threads
;;


let test n =
  let module C = Color in
  work [ C.Blue; C.Yellow; C.Yellow; C.Yellow; C.Yellow; C.Yellow; C.Yellow; C.Yellow; C.Red; ] n;
;;

let ()  =
	Crowbar.(add_test ~name:"Chameneos Test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run (fun () -> test 1000)
	))

