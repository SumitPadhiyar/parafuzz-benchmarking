type id = String.t

type t = (id, Flight.t ref) Hashtbl.t

let make () = Hashtbl.create 128

let addFlight flight table = 
  let (id, _, _, _) = !flight in
    Hashtbl.add table id flight

let getFlightsFor dest table = 
  Hashtbl.fold (fun id flight acc -> let (fid, _, _, _) = !flight in 
                if fid = id then List.append acc [flight]) 
        table []
