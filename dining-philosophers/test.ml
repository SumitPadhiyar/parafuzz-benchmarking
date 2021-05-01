module Crowbar = Parafuzz_lib.Crowbar
open Domain

let num_phils = 5

module Fork = struct
  
  type state = 
    | Available of Condition.t * Mutex.t
    | Unavailable of Condition.t * Mutex.t

  let make () = ref (Available(Condition.create (), Mutex.create ()))

  let take fork = 
    match !fork with
    | Available (cond, mutex) -> 
      Mutex.lock mutex;
      fork := Unavailable (cond, mutex);
      Mutex.unlock mutex
    | Unavailable (cond, mutex) ->
      Mutex.lock mutex;
      Condition.wait cond mutex;
      fork := Unavailable (cond, mutex);
      Mutex.unlock mutex
  
  let return fork = 
    match !fork with
    | Unavailable (cond, mutex) -> Condition.signal cond; fork := Available (cond, mutex)
    | Available (_, _) -> failwith "Unexpected case!"      
    
end


module Philosopher = struct

  type state = 
    | Eating
    | Waiting
    | Thinking
  
  type t = state ref * Fork.state ref * Fork.state ref

  let make lfork rfork = (ref Waiting, lfork, rfork)

  let wait (st, _, _) = 
    let rec wait' t = 
      if t = 0 then ()
      else wait' (t-1)
      in
    st := Waiting;
    wait' (10000)
  
  let eat (st, lf, rf) = 
    match (!st, lf, rf) with
    | (Eating, _, _) -> failwith "Already Eating!"
    | (Thinking, _, _) -> failwith "Not hungry yet!"
    | (Waiting, lf, rf) -> 
      Fork.take lf;
      Fork.take rf;
      st := Eating; wait (st, lf, rf);
      Fork.return lf;
      Fork.return rf      

  let think (st, lf, rf) = 
    let rec think' t = 
      if t = 0 then ()
      else think' (t-1)
      in
    st := Thinking;
    think' (100 * Random.int 100)  

  let rec start' phil iter = 
    if iter = 0 then ()
    else begin
      eat phil;
      think phil;
      wait phil;
      start' phil (iter-1)
    end

  let start phil = start' phil 10

end

let init n = 
  let first_left = ref (Fork.make ()) in
  let first_right = ref (Fork.make ()) in
  let first = first_left in
  let phil_array = ref (Array.make 1 (Philosopher.make !first_left !first_right)) in
  for i = 2 to n-2 do 
    ignore(first_left := !first_right);
    ignore(first_right := (Fork.make ()));
    phil_array := Array.append !phil_array (Array.make 1 (Philosopher.make !first_left !first_right)) 
  done;
  phil_array := Array.append !phil_array (Array.make 1 (Philosopher.make (Fork.make ()) !first));
  Array.iter (fun phil -> ignore(Domain.spawn (fun _ -> Philosopher.start phil))) !phil_array

let ()  = 
  Crowbar.(add_test ~name:"Dining Philosophers Benchmark" [Crowbar.const 1] (fun _ ->
  Parafuzz_lib.run (fun () -> init 10)
  ))
