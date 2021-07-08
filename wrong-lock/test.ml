module Crowbar = Parafuzz_lib.Crowbar

open Domain

module WrongLock = struct
  type t = {
    value : int ref;
    lock : Mutex.t
  }

  let create () = {
    value = ref 0;
    lock = Mutex.create ()
  }

  let modify_var wrongLock mutex = fun () ->
    Mutex.lock mutex;
    wrongLock.value := 1;   (* to be discussed [increment vs constant] *)
    Mutex.unlock mutex

  let modify_str wrongLock = fun () ->
    Mutex.lock wrongLock.lock;
    wrongLock.value := 2;   (* to be discussed [increment vs constant] *)
    Mutex.unlock wrongLock.lock

end

let test () = 
  let wrLock = WrongLock.create () in
  let m = Mutex.create () in
  let d1 = Domain.spawn (WrongLock.modify_var wrLock m) in
  let d2 = Domain.spawn (WrongLock.modify_str wrLock) in 
  Domain.join d1;
  Domain.join d2;
  Crowbar.check (!(wrLock.value) = 2)

let ()  =
  Crowbar.(add_test ~name:"Wrong Lock Test" [Crowbar.const 1] (fun _ ->
    Parafuzz_lib.run test
  ))
  

