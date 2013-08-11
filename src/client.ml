open Bv;;
open Yojson.Safe;;
open Int64;;
open Contest_api;;

module OrderedInt =
  struct
    type t = int
    let compare = Pervasives.compare
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


(* create n random arguments *)
let gen_arguments n =
  Array.map (fun a -> Random.int64 max_int) (Array.init n (fun i -> i))
;;

let answers_equal answers1 answers2 =
  let equal = ref true in
  Array.iteri (fun i e -> if (Int64.compare e answers2.(i)) != 0 then equal := false) answers1;
  !equal
;;

let print_int64_array arr = Array.iter (fun x -> print_endline (Int64.to_string x)) arr;;

(* let pregen_arguments = gen_arguments 256 ;; *)

let pregen_arguments = Array.append [|0L;1L;-1L;7L;15L;255L;-7L;-15L;-255L|] (gen_arguments 247);;

let args_hex = Array.map (fun a -> Printf.sprintf "0x%LX" a) pregen_arguments;;

let generic_solver answers arguments programs =
  let output = Array.map (fun p -> Array.map (eval p) arguments) programs in
(*
  let hey1 = print_endline "======================"; print_newline (); print_newline () in
  let hey2 = Array.iter (fun x -> print_endline (Int64.to_string x)) answers in
*)
  let solution = ref [] in
(*
  print_int64_array arguments;
  print_endline "=============";
  Array.iter print_int64_array output;
*)
  Array.iteri (fun i p -> if answers_equal answers output.(i) then solution := (p::(!solution))) programs;
  !solution
;;



let solver answers programs =
  generic_solver answers pregen_arguments programs
;;

let resolver size answers arguments programs =
  generic_solver answers arguments (Array.of_list programs)
;;
(*
let solve_loop =
  let get_answers 
  solver
*)

let print_problem_stats () =
  let problems_by_size = read_problems () in
  BySize.iter (fun size count -> print_int size; print_string ":"; print_int count; print_newline ()) problems_by_size
;;

let print_sample_search_results () =
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

let command_line_args () =
  match (Array.to_list Sys.argv) with
      argv0::args -> args
    | _ -> invalid_arg "Cannot comprehend the command line arguments."

let rec parse_problem_filters args =
  match args with
      [] -> (fun x -> true)
    | "--size"::n::tail -> (fun x -> x.size == (int_of_string n) && ((parse_problem_filters tail) x))
    | "--solved"::tail -> (fun x -> x.solved && ((parse_problem_filters tail) x))
    | "--unsolved"::tail -> (fun x -> not x.solved && ((parse_problem_filters tail) x))
    | _ -> invalid_arg "Unrecognized problem filter."
;;

let main () =
  match (command_line_args ()) with
      [] ->
        let problem = get_training_problem 9 in
        print_string (problem_to_string problem);
        let ops = problem.operators in
        let programs = gen_programs_all problem.size ops.op1 ops.op2 ops.if0 ops.fold ops.tfold in
        let answers = evaluate problem.id args_hex in
        let solution = solver answers programs in
        let guess_response = guess problem.id (List.nth solution 0) in
        let response_string =
          match guess_response with
            Win -> "Winner!"
          | Mismatch (input, x, y) ->
              "What about: \nInput: " ^ (to_string input) ^ "\nAnswer: "
              ^ (to_string x) ^ "\nYou: " ^ (to_string y)
          | Error error -> "Error! " ^ error in
      (*
        print_endline "==========";
        print_int64_array answers;
        print_int (List.length solution);
        print_newline ();
        print_int (Array.length programs);
      *)
        print_newline ();
        List.iter (fun p -> print_endline (program_to_string p)) solution;
        print_endline response_string
    | ["--get_real_problems"] ->
        ignore (List.map (fun x -> print_string (problem_to_string x)) (get_real_problems ()))
    | "--get_real_problems"::filter_args ->
        ignore (List.map (fun x -> print_string (problem_to_string x)) (get_real_problems_and_filter (parse_problem_filters filter_args)))
    | ["--get_training_problem"] ->
        print_string (problem_to_string (get_training_problem 3))
    | ["--get_training_problem"; int_string] ->
        print_string (problem_to_string (get_training_problem (int_of_string int_string)))
    | _ ->
        print_string "Unrecognized command line arguments."
;;

main ();;

