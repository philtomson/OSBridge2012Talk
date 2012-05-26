module type RING = 
  sig
    type t
    val max : t
    val min : t
    val succ : t -> t
    val val_to_s : t -> string
  end


(* Create a "counter" which is a list of values of type RING *)
module Counter (V_type : RING) = 
  struct 
  include V_type
  let rec next ops = match ops with
      [] -> []
    | o::os -> if o = max then
                 (succ o)::(next os)
               else
                 (succ o)::os 

  let rec max_val vs = List.for_all (fun x -> (x = max)) vs



 let to_s_lst opslst = 
    List.rev (List.map (fun x -> val_to_s x) opslst) 

  let to_s opslst = 
    let list_o_str = to_s_lst opslst in
    String.concat "," list_o_str

  let count_full vs f = 
    let count = ref vs in
    let i = ref 0 in
    while ( not (max_val !count )) do
      (*Printf.printf "%s\n" (to_s !count);*)
      f !count ;
      count := next !count;
      i := !i + 1
    done ;
    (* one last time *)
    (*Printf.printf "%s total: %d\n" (to_s !count) !i;*)
    f !count
      
  end

module Ops = 
  struct
    type t = C | P | M | T | D 

    let max = D

    let min = C

    let val_to_s o = match o with
        C -> "C"
      | P -> "+"
      | M -> "-"
      | T -> "*"
      | D -> "/" 

    let succ o = match o with
      | C -> P
      | P -> M
      | M -> T
      | T -> D  
      | D -> C 

  end

(*
module Binary = 
  struct
    type t = F | T 
    let max = T
    let min = F

    let val_to_s b = match b with 
      F -> "F"
    | T -> "T" 

    let succ b = match b with
      F -> T
    | T -> F

  end
*)


 module OpCounter = Counter(Ops)

(*
  module BinCounter = Counter(Binary)
*)



  open OpCounter
  open Ops

let test = next (next (next (next (next (next (next (next (next (next
[C;C;C]))))))))) ;;
let test_str = to_s test;;
Printf.printf "[%s]\n" (test_str) ;;
count_full [C;C;C;C;C;C;C;C;C] (fun x -> Printf.printf "%s\n" (to_s x) );;

(*
  open BinCounter
  open Binary
let test = next (next (next (next (next (next (next (next (next (next
[F;F;F;F]))))))))) ;;
let test_str = to_s test;;
Printf.printf "[%s]\n" (test_str) ;;
count_full [F;F;F] ;;
*)
