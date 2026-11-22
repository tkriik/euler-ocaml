(* lockfree stack
module Stack : sig
  type 'a t

  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
end = struct
  type 'a t = 'a list Atomic.t

  let make () = Atomic.make []

  let rec push r v =
    let s = Atomic.get r in
    if Atomic.compare_and_set r s (v :: s)
    then ()
    else (
      Domain.cpu_relax ();
      push r v)
  ;;

  let rec pop r =
    let s = Atomic.get r in
    match s with
    | [] -> None
    | x :: xs ->
      if Atomic.compare_and_set r s xs
      then Some x
      else (
        Domain.cpu_relax ();
        pop r)
  ;;
end
*)

(* --------------
let r = Atomic.make None
let sender () = Atomic.set r (Some "hi")

let rec receiver () =
  match Atomic.get r with
  | None ->
    Domain.cpu_relax ();
    receiver ()
  | Some msg -> print_endline msg
;;

let main () =
  let s = Domain.spawn sender in
  let d = Domain.spawn receiver in
  Domain.join s;
  Domain.join d
;;

let _ = main ()
*)

(* -------------
open Printf

let twice_par f =
  let d1 = Domain.spawn f in
  let d2 = Domain.spawn f in
  Domain.join d1;
  Domain.join d2
;;

let plain_ref n =
  let r = ref 0 in
  let f () =
    for _ = 1 to n do
      incr r
    done
  in
  twice_par f;
  printf "Non-atomic ref count: %d\n" !r
;;

let atomic_ref n =
  let r = Atomic.make 0 in
  let f () =
    for _ = 1 to n do
      Atomic.incr r
    done
  in
  twice_par f;
  printf "Atomic ref count: %d\n" (Atomic.get r)
;;

let main () =
  let n =
    try int_of_string Sys.argv.(1) with
    | _ -> 1
  in
  plain_ref n;
  atomic_ref n
;;

let _ = main ()
*)

(* threads
open Printf

let m = Mutex.create ()
let r = ref None

let task () =
  let my_thread_id = Thread.(id (self ())) in
  let my_domain_id :> int = Domain.self () in
  Mutex.lock m;
  (match !r with
   | None ->
     printf
       "Thead %d running on domain %d saw initial write\n%!"
       my_thread_id
       my_domain_id
   | Some their_thread_id ->
     printf
       "Thread %d running on domain %d saw the write by thread %d\n%!"
       my_thread_id
       my_domain_id
       their_thread_id);
  r := Some my_thread_id;
  Mutex.unlock m
;;

let task' () =
  let t = Thread.create task () in
  task ();
  Thread.join t
;;

let main () =
  let d = Domain.spawn task' in
  task' ();
  Domain.join d
;;

let _ = main ()
*)

(* blocking stack
module Stack : sig
  type 'a t

  val make : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end = struct
  type 'a t =
    { mutable contents : 'a list
    ; mutex : Mutex.t
    ; nonempty : Condition.t
    }

  let make () = { contents = []; mutex = Mutex.create (); nonempty = Condition.create () }

  let push r v =
    Mutex.lock r.mutex;
    r.contents <- v :: r.contents;
    Condition.signal r.nonempty;
    Mutex.unlock r.mutex
  ;;

  let pop r =
    Mutex.lock r.mutex;
    let rec loop () =
      match r.contents with
      | [] ->
        Condition.wait r.nonempty r.mutex;
        loop ()
      | x :: xs ->
        r.contents <- xs;
        x
    in
    let res = loop () in
    Mutex.unlock r.mutex;
    res
  ;;
end
*)

(* fib_par2.ml
let num_domains = int_of_string Sys.argv.(1)
let n = int_of_string Sys.argv.(2)
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

module T = Domainslib.Task

let rec fib_par pool n =
  if n > 20
  then (
    let a = T.async pool (fun _ -> fib_par pool (n - 1)) in
    let b = T.async pool (fun _ -> fib_par pool (n - 2)) in
    T.await pool a + T.await pool b)
  else fib n
;;

let main () =
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let res = T.run pool (fun _ -> fib_par pool n) in
  T.teardown_pool pool;
  Printf.printf "fib(%d) = %d\n" n res
;;

let _ = main ()
*)
