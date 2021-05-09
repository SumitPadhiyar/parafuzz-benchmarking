let t = Customer.t ref list * int

let sellTickets customers table = 
  let sellTicket customer = match !customer with begin
    | Waiting(fname, lname, dest) -> 
            let checkAndSell flight = 
              (let freeSeats = Flight.getFreeSeats flight in
              let sell seat = 
                (if (FlightTicket.isFree seat && Customer.isWaiting customer) 
                then FlightTicket.buy fname lname seat else ()) in 
              List.iter sell freeSeats) in 
            let flights = FlightsTable.getFlightsFor dest table in
              List.iter checkAndSell flights
    | _ -> ()

