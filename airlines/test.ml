module Crowbar = Parafuzz_lib.Crowbar
open Domain

let random n = Crowbar.sample_from_generator (Crowbar.range n)

let slice arr l r = 
  let res = [] in
  List.iteri (fun i elem -> if (i >= l && i<=r) then List.append res elem else ()) arr

let test () = 
  let destinations = ["BLR"; "CHN"; "HYD"; "DEL"; "BOM"; "TRI"] in
  let flightsTable = ref (FlightsTable.make ()) in
  let _ = List.init (random 10) (fun i -> Airlines.make ("Airport - " ^ (string_of_int i) flightsTable) destinations) in
  let customers = List.init (random 1000) (fun i -> ref (Customer.make ("Passenger - ", (string_of_int i)))) in
  let domains = List.init 5 (Domain.spawn (fun () -> Seller.sellTickets customers flightsTable)) in   
      (* TODO: split the customer list randomly and assign each domain a few, and then test *)
  List.iter (Domain.join) domains


let _ = Crowbar.(add_test ~name:"Airlines benchmark" [Crowbar.const 1] (fun _ ->
              Parafuzz_lib.run test
          ))
  
