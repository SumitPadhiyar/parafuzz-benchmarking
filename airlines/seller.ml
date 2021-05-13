module Crowbar = Parafuzz_lib.Crowbar
open Customer

let random n = Crowbar.sample_from_generator (Crowbar.range n)


let sellTickets customers table = 
  let sellTicket customer = match !customer with
    | Waiting(fname, lname, dest) -> 
            let checkAndSell flight = 
              (let freeSeats = Flight.getFreeSeats flight in
              let sell seat = 
                (if (FlightTicket.isFree seat && Customer.isWaiting customer) 
                then (FlightTicket.buy fname lname seat; Customer.setTicket customer seat) else ()) in 
              List.iter sell freeSeats) in 
            let flights = FlightsTable.getFlightsFor dest table in
              List.iter checkAndSell flights
    | _ -> ()
  in 
  List.iter sellTicket customers

let checkTickets customers = 
  let checkRemaining = List.fold_left (fun acc c -> match !c with | Waiting(_, _, _) -> acc+1 | _ -> acc) 0 in
    if (checkRemaining customers) > 4 then failwith "Insufficient tickets!" else ()

let run table = 
  let destinations = ["BLR"; "CHN"; "HYD"; "DEL"; "BOM"; "TRI"] in
  let customers = List.init (random 20) (fun i -> ref (Customer.make "Passenger - " (string_of_int i) 
                                              (List.nth destinations (random (List.length destinations))))) in
  sellTickets customers table;
  checkTickets customers

