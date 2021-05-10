module Crowbar = Parafuzz_lib.Crowbar
open Domain

let random n = Crowbar.sample_from_generator (Crowbar.range n)

let test () = 
  let destinations = ["BLR"; "CHN"; "HYD"; "DEL"; "BOM"; "TRI"] in
  let flightsTable = (FlightsTable.make ()) in
  let _ = List.init (random 10) (fun i -> Airline.make ("Airport - " ^ (string_of_int i)) flightsTable destinations) in
  let domains = List.init 5 (fun _ -> Domain.spawn (fun () -> Seller.run flightsTable)) in
  List.iter (Domain.join) domains

let _ = Crowbar.(add_test ~name:"Airlines benchmark" [Crowbar.const 1] (fun _ ->
              Parafuzz_lib.run test
          ))
  
