module Crowbar = Parafuzz_lib.Crowbar
open Domain

let rand i = 
  if i mod 75037 = 0 then -1 else 1 (* Replace 75037 with any large prime number *)

let range n = Crowbar.sample_from_generator (Crowbar.range n)

module Bank = struct

  let make () = 
    let records:(int, int) Hashtbl.t = Hashtbl.create 128 in
    (records, ref 0)

  let service id deposit bank = 
    let _ = Hashtbl.replace (fst(!bank)) id ((Hashtbl.find (fst(!bank)) id) + deposit) in
    let bal = !(snd(!bank)) in
    snd(!bank) := bal + deposit;
    if !(snd(!bank)) < 0 then failwith "Negative balance!\n"

  let register id bank =
    Hashtbl.add (fst(!bank)) id 0

  let get_balance id bank = 
    Hashtbl.find (fst(!bank)) id

end


module Account = struct

  let rec loop f iters = 
    if iters = 0 then () 
    else let () = f () in loop f (iters-1)

  let register id bank = 
    Bank.register id bank; loop (fun () -> Bank.service id ((range 1000) * rand (range 10000))  bank) (range 100)

end


let test n =
  let bank = ref (Bank.make ()) in
    let acc_arr = Array.init (abs(n mod 128)) (fun i -> (Domain.spawn (fun () -> Account.register i bank))) in
    let _ = Array.iter (Domain.join) acc_arr in
    let records = fst(!bank) in
    let total = snd(!bank) in
    let balance = ref 0 in
    let _ = Hashtbl.iter (fun id sum -> balance := !balance + sum) records in
    if !total = !balance then () 
    else Printf.printf "Something wrong - %d %d\n" !total !balance


let () = Crowbar.(add_test ~name:"Bank benchmark" [int] ( fun i ->
      Parafuzz_lib.run (fun () -> test i)
    ))
