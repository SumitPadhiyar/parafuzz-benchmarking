type id = String.t

type t = (id, Flight.t ref) Hashtbl.t

let make () : t = Hashtbl.create 128

let addFlight flight table = 
  let (id, _, _, _) = !flight in
    Hashtbl.add table id flight

let getFlightsFor dest (table:t) = 
  Hashtbl.fold (fun id flight acc -> let (_, _, dst, _) = !flight in 
                if dst = dest then List.append acc [!flight] else acc) 
        table []
