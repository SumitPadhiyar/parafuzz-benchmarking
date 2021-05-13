module Crowbar = Parafuzz_lib.Crowbar

let range n = Crowbar.sample_from_generator (Crowbar.range n)

type name = String.t

type dests = String.t list

type t = name * dests

let make name table destinations =
  let len = List.length destinations in
  let populate i dest = begin
    let id = "Flight " ^ (string_of_int i) in
    let flight = Flight.make id dest (List.nth destinations (range len)) ((range 10)) in
    FlightsTable.addFlight (ref flight) table
  end in
  List.iteri populate destinations



  