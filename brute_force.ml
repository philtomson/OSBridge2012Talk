(* Inspired by this puzzle from NPR:
 http://www.npr.org/2012/01/29/146034893/this-puzzle-is-the-pits 
 (see the 'Next Weeks' challenge section there):
 "Next Week's Challenge from listener Ed Pegg Jr.: Write the digits from 
 1 to 9 in a line. If you put times signs after the 2 and 4, a plus 
 sign after the 5, and a minus sign after the 7, you have 
 12 x 34 x 5 + 67 - 89, which equals 2018.
 That's six years off from our current year 2012. This example uses 
 four arithmetic symbols. The object is to use just three of the 
 following arithmetic operations: addition, subtraction, multiplication 
 and division, in a line from 1 to 9 to get 2012 exactly. The operations 
 should be performed in order from left to right" *)

(* 
 * Brute force implementaion 
 *)

(* This program solves the puzzle above using a genetic algorithm *)

module type RING = 
  sig
    type t
    val max : t
    val min : t
    val succ : t -> t
    val val_to_s : t -> string
  end

(* Create a "counter" which is a list of values of type RING *)
module Counter (V_type : RING) =  (* this is a functor *)
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
      f !count ;
      
  end

module Ops = 
  struct
    type t = C | P | M | T | D 

    let max = C

    let min = P

    let val_to_s o = match o with
        C -> "C"
      | P -> "+"
      | M -> "-"
      | T -> "*"
      | D -> "/" 

      (*
    let succ o = match o with
      | C -> P
      | P -> M
      | M -> T
      | T -> D  
      | D -> C 
      *)
    let succ o = match o with
      | C -> P
      | P -> M
      | M -> T
      | T -> D  
      | D -> C 

  end

  module OpCounter = Counter(Ops)
  open OpCounter
  open Ops

let cmd_gens = ref 5000
let cmd_seed = ref 42
let usage = "usage: " ^ Sys.argv.(0) ^ " [-g int] [-s int]"

let speclist = [
    ("-g", Arg.Int (fun d -> cmd_gens := d),": generations: int param ");
    ("-s", Arg.Int (fun d -> cmd_seed := d),": seed: int param");
  ]
 
let () =
  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;;

let nums = [1;   2;   3;   4;   5;   6;   7;   8;   9]
let num_size   = List.length nums
let ops_size = num_size - 1 
let target = 2012
let mutation_rate = 0.10 

exception SizeMismatch
exception FoundIt

(* C + - * /  *)
let num_to_op rn = match rn with 
         0  -> "C"
       | 1  -> "+"
       | 2  -> "-"
       | 3  -> "*"
       | 4  -> "/"   
       | _  -> "C" 

(* combine the number list with the op list *)
let combine n_lst op_lst  = 
  let op_lst_len = List.length op_lst in
  let rec build' n_lst' outlst i = match n_lst' with 
      []    ->  List.rev outlst
   |  n::ns ->  if i < op_lst_len then
                  build' ns ( (List.nth op_lst i)::n::outlst) (i+1) 
                else 
                  build' ns ( n::outlst) (i+1) in
   build' n_lst [] 0 ;;

let cat n_oplst =
  let rec do_cat nolst outs accum = match nolst with
     []   -> List.rev outs
  |  n::op::nos -> if op = "C" then
                     do_cat nos outs (accum ^ n) 
                   else 
                     do_cat nos (op::(accum^n)::outs) "" 
  | n::[]       -> do_cat [] ((accum^n)::outs) (accum^n)
                   in
     do_cat n_oplst [] "";;

exception BadNumOpFormat

let do_ops lst = 
  let rec mdps nlst accum oper = 
  match nlst with
    []         -> accum
  | n::op::ns  -> 
                  let n' = int_of_string n in
                  (match op with
                    "*" -> mdps ns (oper accum n') ( * )
                  | "/" -> mdps ns (oper accum n') ( / )
                  | "+" -> mdps ns (oper accum n') ( + )
                  | "-" -> mdps ns (oper accum n') ( - )
                  | _   -> raise BadNumOpFormat
                  )
  |  n::[]     -> let n' = int_of_string n in 
                  mdps [] (oper accum n') oper in
  mdps lst 1 ( * ) (* multiply by 1 is identity *) 

let ( |> ) a b = b a 
exception EvalError

let eval lst = 
  let lst' = (combine (List.map (fun x -> string_of_int x) nums) lst) in
  (cat lst' |> do_ops ) 

let delta t n =  abs (t - n)  

let rank_pop pop = List.sort (fun a b -> 
                                if (delta target (fst a)) > (delta target (fst b)) then
                                  1
                                 else if a = b then 
                                  0
                                 else -1
                   ) (List.map (fun e -> (eval e) , e ) pop) 

let run_all () = 
  (*let ops = ["C";"+";"-";"*";"/"] in *)
  (*let initial_ops = [P;P;P;P;P;P;P;P] in*)
  let initial_ops = List.rev [P;C;C;C;T;D;M;P] in
  count_full initial_ops (fun x -> 
                            let op_lst = (to_s_lst x) in
                            let res = eval op_lst in
                              if res = target then
                              (
                                Printf.printf "We Have a winner: %s = %d\n"
                                (String.concat "," op_lst) res;
                                raise FoundIt;
                              )
                              else
                              ( 
                                (*Printf.printf " %s = %d\n"
                                (String.concat "," op_lst) res;
                                raise FoundIt;
                                *)
                              )
                         )
                            

 let _ = Printf.printf "Now try brute force method...\n";;
run_all () ;;


