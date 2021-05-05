module Crowbar = Parafuzz_lib.Crowbar

type bank_account = {mutable balance: int; lock: Domain.Mutex.t}

let get_balance account = let ret = ref 0 in
    Domain.Mutex.lock account.lock;
    ret := account.balance;
    Domain.Mutex.unlock account.lock;
    !ret

let set_balance account bal = 
    Domain.Mutex.lock account.lock;
    account.balance <- bal;
    Domain.Mutex.unlock account.lock

let withdraw account amt = let curr_bal = ref 0 in
    curr_bal := get_balance account;
    curr_bal := !curr_bal - amt;
    set_balance account !curr_bal 

let deposit account amt = let curr_bal = ref 0 in
    curr_bal := get_balance account;
    curr_bal := !curr_bal + amt;
    set_balance account !curr_bal

let init_account account = account.balance <- 0

let d1 account amt = 
(*     printf "d1 is depositing %d\n" amt; *)
    deposit account amt
(*     printf "d1 deposit made\n" *)

let d2 account amt =
(*     printf "d2 is withdrawing %d\n" amt; *)
    withdraw account amt
(*     printf "d2 withdrawal done\n" *)


let test amt =
    let account = {balance = 0; lock = Domain.Mutex.create ()} in
    init_account account;
    
    let d1 = Domain.spawn (fun () -> d1 account amt) in
    let d2 = Domain.spawn (fun () -> d2 account amt) in
    Domain.join d1;
    Domain.join d2;
    Crowbar.check (account.balance = 0)

let _  = 
    let amt = 20 in
	Crowbar.(add_test ~name:"Bank acount maple test" [Crowbar.const 1] (fun _ ->
		Parafuzz_lib.run  (fun () -> test amt)
	))
