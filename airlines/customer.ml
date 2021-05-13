type fname = String.t 

type lname = String.t 

type dest = String.t

type t = 
  | Waiting of fname * lname * dest
  | Booked of fname * lname * dest * (FlightTicket.t ref)

let make fname lname dest = Waiting(fname, lname, dest)

let isWaiting customer = match !customer with
| Waiting (_, _, _) -> true
| _ -> false

let setTicket customer ticket = match !customer with
| Waiting (f, l, d) -> customer := Booked(f, l, d, ticket)
| _ -> failwith "Already bought ticket!\n"

