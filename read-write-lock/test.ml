module Crowbar = Parafuzz_lib.Crowbar

open Domain

module RWLock = struct

  type t = {
    mutable data : int;
    rlock : Mutex.t;
    wlock : Mutex.t;
    r_waitlock : Mutex.t;
    w_waitlock : Mutex.t;
    can_read : Condition.t;
    can_write : Condition.t;
    mutable reading : bool;
    mutable writing : bool;
  }

  let create () = 
    {
      data = 0;
      rlock = Mutex.create ();
      wlock = Mutex.create ();
      r_waitlock = Mutex.create ();
      w_waitlock = Mutex.create ();
      can_read = Condition.create ();
      can_write = Condition.create ();
      reading = false;
      writing = false
    }

  let get_read_lock rwLock = 
    if rwLock.writing = true then
    (Mutex.lock (rwLock.r_waitlock);
    Condition.wait (rwLock.can_read) (rwLock.r_waitlock);
    Mutex.unlock (rwLock.r_waitlock);
    rwLock.rlock)
    else
    rwLock.rlock

  let get_write_lock rwLock = 
    if rwLock.reading = true then
    (Mutex.lock (rwLock.w_waitlock);
    Condition.wait (rwLock.can_write) (rwLock.w_waitlock);
    Mutex.unlock (rwLock.w_waitlock);
    rwLock.wlock)
    else
    rwLock.wlock

  let read rwLock = 
    let m = get_read_lock rwLock in
    rwLock.reading <- true;
    Mutex.lock m;
    Printf.printf "%d\n" rwLock.data;
    Mutex.unlock m;
    rwLock.reading <- false;
    Condition.signal (rwLock.can_write)

  let write rwLock = 
    let m = get_write_lock rwLock in
    rwLock.writing <- true;
    Mutex.lock m;
    rwLock.data <- 10;
    Mutex.unlock m;
    rwLock.writing <- false;
    Condition.signal (rwLock.can_read)

  let read_write rwLock = 
    let m = get_read_lock rwLock in
    rwLock.reading <- true;
    Mutex.lock m;
    Printf.printf "%d\n" rwLock.data;
    Mutex.unlock m;
    let m1 = get_write_lock rwLock in
    rwLock.writing <- true;
    Mutex.lock m1;
    rwLock.data <- 10;
    Mutex.unlock m1;
    rwLock.writing <- false;
    Condition.signal (rwLock.can_read);
    rwLock.reading <- false;
    Condition.signal (rwLock.can_write)

end


let test () = 
  let rwLock = RWLock.create () in
  let d1 = Domain.spawn (fun () -> RWLock.read_write rwLock) in
  Domain.join d1
  
let () = 
  Crowbar.(add_test ~name:"Read Write Deadlock Benchmark" [Crowbar.const 1] (fun _ -> 
    Parafuzz_lib.run test
  ))
