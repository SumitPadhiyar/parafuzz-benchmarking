module T = Domainslib.Task
module Crowbar = Parafuzz_lib.Crowbar

let num_domains = 4
let n = 10

let print_arr arr = 
  for i = 0 to Array.length arr - 1 do
    print_int arr.(i);
    print_string "  "
    done

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let partition arr low high =
  let x = arr.(high) and  i = ref (low-1) in
  if (high-low > 0) then
    begin
      for j= low to high - 1 do
        Domain.Sync.poll ();
        if arr.(j) <= x then
          begin
            i := !i+1;
            swap arr !i j
          end
      done
    end;
    swap arr (!i+1) high;
    !i+1

let rec quicksort arr low high d pool =
  if (high - low) <= 0 then ()
  else begin
    if d > 1 then begin
      let q = partition arr low high in
      let c = T.async pool (fun () -> quicksort arr low (q-1) (d/2) pool) in
      quicksort arr (q+1) high (d/2 + (d mod 2)) pool;
      T.await pool c
    end else begin
      let q = partition arr low high in
      quicksort arr low (q-1) d pool;
      quicksort arr (q+1) high d pool;
    end
  end

let test () =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) in
  let arr = Array.init n (fun _ -> Random.int n) in
  quicksort arr 0 (Array.length arr - 1) num_domains pool;
  Printf.printf "\n";
  print_arr arr;
  Printf.printf "\n";
  T.teardown_pool pool


let ()  = 
  Crowbar.(add_test ~name:"Quick Sort" [Crowbar.const 1] (fun _ ->
  Parafuzz_lib.run test
  ))
