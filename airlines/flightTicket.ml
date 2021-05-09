type seatnum = int

type src = String.t

type dst = String.t

type t = 
  | Free of seatnum
  | Booked of src * dst * seatnum

let make seat = Free(seat)

let isFree seat = match !seat with
| Free (_) -> true
| _ -> false

let buy fname lname seat = match !seat with
| Free (x) -> seat := Booked(fname, lname, x)
| _ -> failwith "Seat already bought!"
