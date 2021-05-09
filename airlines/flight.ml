type id = String.t

type src = String.t

type dst = String.t

type ticket = FlightTicket.t ref

type t = id * src * dst * ticket list

let make id src dst size = 
  let tickets = List.init size (fun i -> (ref (FlightTicket.make i)) ) in
  (id, src, dst, tickets)

let getFreeSeats (id, src, dst, tickets) = 
  let f acc ticket = 
    match !ticket with begin
    | Free (x) -> List.append acc [x]
    | _ -> acc 
    end in
  List.fold_left f [] tickets    
