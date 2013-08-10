open Bv;;
open Yojson.Safe;;

module OrderedInt =
  struct
    type t = int
    let compare = compare
  end;;

module BySize = Map.Make(OrderedInt);;

let read_problems () = 
  let problems = from_file "problems.json" in
  let arr = match problems with
    `List x -> x
  | _ -> [] in
  let size problem = 
    let size_json = match problem with
      `Assoc (xs) -> List.find (function (key, x) -> key = "size") xs
    | _ -> ("size", `Null) in
    match size_json with
      (_, `Int i) -> i
    | _ -> 0 in
  let increment map problem =
    let problem_size = size problem in
    if BySize.mem problem_size map
    then BySize.add problem_size ((BySize.find problem_size map) + 1) map
    else BySize.add problem_size 1 map in
  List.fold_left increment (BySize.empty) arr 
;;


(* create 256 random arguments *)
let gen_arguments () =
  Array.map (fun a -> Random.int64 max_int) (Array.init 255 (fun i -> i))
;;

let pregen_arguments = gen_arguments () ;;

let solver size op1s op2s if0 fold tfold =
  let candidates = gen_programs_all size op1s op2s if0 fold tfold in
  let answers = Array.map (fun p -> Array.map (eval p) pregen_arguments) candidates in
  let args_hex = Array.map (fun a -> Printf.sprintf "0x%LX" a) pregen_arguments in
  args_hex
;;

let main () =
(*
  let problems_by_size = read_problems () in
  BySize.iter (fun size count -> print_int size; print_string ":"; print_int count; print_newline ()) problems_by_size
*)
  print_int (List.length (gen_pseudo 11 true false false));
  print_newline ();
  print_int (List.length (gen_pseudo 11 false true false));
  print_newline ();
  print_int (List.length (gen_pseudo 11 true true false));
  print_newline ();
  print_int (List.length (gen_pseudo 11 true false true));
  print_newline ();
  print_int (List.length (gen_pseudo 11 false false true));
  print_newline ();
;;

main ();;

