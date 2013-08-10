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

