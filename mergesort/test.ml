module T = Domainslib.Task
module Crowbar = Parafuzz_lib.Crowbar

let num_domains = 4
let arr_size = 10
let min = 5

let _ = Random.init 123
let a = Array.init arr_size (fun _ -> Random.int arr_size)
let b = Array.make arr_size 0

type array_slice = {arr: int array; index: int; length: int}

let print_array_slice a =
  Printf.printf ("===========================\n");
  for i = a.index to a.index + a.length - 1 do
    Printf.printf "%d " a.arr.(i)
  done;
  Printf.printf ("===========================\n")


let sort a =
  for i = a.index to a.index + a.length - 2 do
    for j = i + 1 to a.index + a.length - 1 do
      if a.arr.(j) < a.arr.(i) then
        let t = a.arr.(i) in
        a.arr.(i) <- a.arr.(j);
        a.arr.(j) <- t
    done
  done

let merge a b res =
  let rec loop ai bi ri =
    match a.index + a.length - ai, b.index + b.length - bi with
    | arr_size, 0 -> Array.blit a.arr ai res.arr ri arr_size
    | 0, arr_size -> Array.blit b.arr bi res.arr ri arr_size
    | _, _ ->
        if a.arr.(ai) < b.arr.(bi) then begin
          res.arr.(ri) <- a.arr.(ai);
          loop (ai+1) bi (ri+1)
        end else begin
          res.arr.(ri) <- b.arr.(bi);
          loop ai (bi+1) (ri+1)
        end
  in
  loop a.index b.index res.index

let merge_sort a b l =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  Printf.printf "Pool setup!\n";
  print_array_slice a;
  print_array_slice b;
  let rec merge_sort' a b l =
    let () = Printf.printf "Before anything started!\n" in
    if a.length <= min then begin
      sort a; a
    end else 
      let a1 = {a with index = a.index; length = a.length / 2} in

      let b1 = {b with index = b.index; length = b.length / 2} in

      let () = Printf.printf "Before Task 1 started!\n" in

      let r1 = T.async pool (fun _ -> merge_sort' a1 b1 (2*l+1)) in

      Printf.printf "Task 1 started!\n";

      let a2 = {a with index = a.index + a.length / 2;
                  length = a.length - a.length / 2} in

      let b2 = {b with index = b.index + b.length / 2;
                  length = b.length - b.length / 2} in

      let () = Printf.printf "Before Task 1 started!\n" in

      let r2 = T.async pool (fun _ -> merge_sort' a2 b2 (2*l+2)) in

      Printf.printf "Task 2 started!\n";

      let (r1, r2) = (T.await pool r1, T.await pool r2) in

      Printf.printf "Tasks done!\n";

      if r1.arr != r2.arr then begin
        if r2.arr == a.arr then begin
          Printf.printf "Case 1\n";
          Printf.printf "Case 1 merging done!\n";
          merge r1 r2 a;
          a
        end else begin
          Printf.printf "Case 2\n";
          Printf.printf "Case 2 merging done!\n";
          merge r1 r2 b;
          b
        end
      end else if r1.arr == a.arr then begin
        Printf.printf "Case 3\n";
        merge r1 r2 b;
        Printf.printf "Case 3 merging done!\n";
        b
      end else begin
        Printf.printf "Case 4\n";
        merge r1 r2 a;
        Printf.printf "Case 4 merging done!\n";
        a
      end in
  let () = Printf.printf "Before mergesort is started!\n" in
  ignore @@ merge_sort' a b l;
  let () = Printf.printf "After mergesort is done!\n" in
  T.teardown_pool pool

let test () =
  let aslice = {arr = a; index = 0; length = arr_size} in
  let bslice = {arr = b; index = 0; length = arr_size} in
  let _ = merge_sort aslice bslice 0 in
  print_array_slice aslice;
  Printf.printf "=====================  Merge sort done! ====================="

let ()  = 
  Crowbar.(add_test ~name:"Merge Sort" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))
