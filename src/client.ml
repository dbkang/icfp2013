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

let pregen_arguments_simple = [|0L;1L;-1L;7L;15L;255L;-7L;-15L;-255L|];;
let pregen_arguments = Array.append [|0L;1L;-1L;7L;15L;255L;-7L;-15L;-255L|] (gen_arguments 247);;

let args_to_hex = Array.map (fun a -> Printf.sprintf "0x%LX" a);;

let args_hex = args_to_hex pregen_arguments;;

let solver answers arguments programs =
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


let resolver size answers arguments programs =
  solver answers arguments (Array.of_list programs)
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
    | "--failed"::tail -> (fun x -> x.finished && not x.solved && ((parse_problem_filters tail) x))
    | "--id"::id::tail -> (fun x -> x.id = id && ((parse_problem_filters tail) x))
    | "--size"::n::tail -> (fun x -> x.size = (int_of_string n) && ((parse_problem_filters tail) x))
    | "--solved"::tail -> (fun x -> x.solved && ((parse_problem_filters tail) x))
    | "--tfold"::tail -> (fun x -> x.operators.tfold && ((parse_problem_filters tail) x))
    | "--unsolved"::tail -> (fun x -> not x.solved && not x.finished && ((parse_problem_filters tail) x))
    | "--no_bonus"::tail -> (fun x -> not x.operators.bonus && ((parse_problem_filters tail) x))
    | _ -> invalid_arg "Unrecognized problem filter."
;;


let run_problem_solver_speculative problem =
  let ops = problem.operators in
  let answers = evaluate problem.id args_hex in
  gen_programs_partial_do problem.size ops.op1 ops.op2 ops.if0 ops.fold ops.tfold (fun programs ->
    match solver answers pregen_arguments programs with
      hd::tl ->
        (match guess problem.id hd with
          Win -> print_endline ("Behold the power of guessing: " ^ (program_to_string hd)); true
        | Mismatch (input, x, y) -> false
        | Error error -> false)
    | _ -> false);
  ()
;;

let run_problem_solver problem tries =
  let ops = problem.operators in
  let programs = gen_programs_all problem.size ops.op1 ops.op2 ops.if0 ops.fold ops.tfold in
  let answers = evaluate problem.id args_hex in
  let rec iter tries answers arguments programs = 
    let solution = solver answers arguments programs in
    match solution with
      [] ->
        print_endline "Something's wrong, no matching program was found.  Retrying with fewer args";
        let args = gen_arguments 5 in
        iter tries (evaluate problem.id (args_to_hex args)) args programs
    | hd::tl ->
        let guess_response = guess problem.id hd in
        let response_string =
          match guess_response with
            Win -> "Winner!"
          | Mismatch (input, x, y) ->
              "What about: \nInput: " ^ (to_string input) ^ "\nAnswer: "
              ^ (to_string x) ^ "\nYou: " ^ (to_string y)
          | Error error -> "Error! " ^ error in
        begin
          List.iter (fun p -> print_endline (program_to_string p)) solution;
          print_endline response_string;
          match (guess_response, tries) with
            (Mismatch(_, _, _), 0)  -> failwith "Tried too many times, giving up"
          | (Mismatch(input, answer, _), n) -> iter (n - 1) [|answer|] [|input|] (Array.of_list solution)
          | _ -> ();
        end in
  iter tries answers pregen_arguments programs
;;

let training_solver size tries =
  let problem = get_training_problem size in
  print_string (problem_to_string problem); print_newline ();
  if (size > 11) then
    run_problem_solver_speculative problem
  else
    run_problem_solver problem tries
;;

let training_solver_no_fold size tries =
  let problem = get_training_problem_without_folds size in
  print_string (problem_to_string problem); print_newline ();
  if (size > 11) then
    run_problem_solver_speculative problem
  else
    run_problem_solver problem tries
;;

let training_solver_with_fold size tries =
  let problem = get_training_problem_with_fold size in
  print_string (problem_to_string problem); print_newline ();
  if (size > 11) then
    run_problem_solver_speculative problem
  else
    run_problem_solver problem tries
;;

let training_solver_with_tfold size tries =
  let problem = get_training_problem_with_tfold size in
  print_string (problem_to_string problem); print_newline ();
  if (size > 14) then
    run_problem_solver_speculative problem
  else
    run_problem_solver problem tries
;;

(* For reference, the solution is:
  (lambda (x_7568) (fold (plus 0 x_7568) 0 (lambda (x_7569 x_7570) (xor (shl1 x_7569) x_7570))))
*)
let test_problem = {
  id = "TSYXnQEQoi8ejGD8wxjyayfp";
  size = 11;
  operators = {
    op1 = [ Shl1 ];
    op2 = [ Plus; Xor ];
    if0 = false;
    fold = true;
    tfold = false;
    bonus = false
  };
  solution = "";
  solved = false;
  finished = false
};;

type user_command = SolveProblem | SkipProblem | QuitSolving;;

let rec get_command () =
  print_string "Solve this problem (solve/skip/quit)?  ";
  match (read_line ()) with
      "solve" -> SolveProblem
    | "skip"  -> SkipProblem
    | "quit"  -> QuitSolving
    | _ -> (get_command ())
;;

let solve_problem problem =
  print_string "Candidate problem:\n";
  print_string (problem_to_string problem);
  match (get_command ()) with
      SolveProblem -> run_problem_solver problem 10
    | SkipProblem  -> print_string "\n"
    | QuitSolving  -> exit 0
;;

let solve_problem_speculatively problem =
  print_string "Candidate problem:\n";
  print_string (problem_to_string problem);
  match (get_command ()) with
      SolveProblem -> run_problem_solver_speculative problem
    | SkipProblem  -> print_string "\n"
    | QuitSolving  -> exit 0
;;

let solve_problem_no_confirm problem =
  print_endline "Candidate problem:\n";
  print_endline (problem_to_string problem);
  run_problem_solver problem 10
;;

let solve_problem_speculatively_no_confirm problem =
  print_endline "Candidate problem:\n";
  print_endline (problem_to_string problem);
  run_problem_solver_speculative problem
;;

let main () =
  match (command_line_args ()) with
    [] -> training_solver 10 5
  | ["--solve_training_problem"; size; tries] -> training_solver (int_of_string size) (int_of_string tries)
  | ["--solve_training_problem_without_folds"; size; tries] -> training_solver_no_fold (int_of_string size) (int_of_string tries)
  | ["--solve_training_problem_with_fold"; size; tries] -> training_solver_with_fold (int_of_string size) (int_of_string tries)
  | ["--solve_training_problem_with_tfold"; size; tries] -> training_solver_with_tfold (int_of_string size) (int_of_string tries)
  | ["--get_real_problems"] ->
      ignore (List.map (fun x -> print_string (problem_to_string x)) (get_real_problems ()))
  | "--get_real_problems"::filter_args ->
      ignore (List.map (fun x -> print_string (problem_to_string x)) (get_real_problems_and_filter (parse_problem_filters filter_args)))
  | ["--get_training_problem"] ->
      print_string (problem_to_string (get_training_problem 3))
  | ["--get_training_problem"; int_string] ->
      print_string (problem_to_string (get_training_problem (int_of_string int_string)))
  | ["--get_training_problem_without_folds"; int_string] ->
      print_string (problem_to_string (get_training_problem_without_folds (int_of_string int_string)))
  | ["--get_training_problem_with_fold"; int_string] ->
      print_string (problem_to_string (get_training_problem_with_fold (int_of_string int_string)))
  | ["--get_training_problem_with_tfold"; int_string] ->
      print_string (problem_to_string (get_training_problem_with_tfold (int_of_string int_string)))
  | "--solve_real_problems"::filter_args ->
      ignore (List.map solve_problem (get_real_problems_and_filter (parse_problem_filters ("--unsolved"::filter_args))))
  | "--solve_real_problems_speculatively"::filter_args ->
      ignore (List.map solve_problem_speculatively (get_real_problems_and_filter (parse_problem_filters ("--unsolved"::filter_args))))
  | "--solve_real_problems!!!"::filter_args ->
      ignore (List.map solve_problem_no_confirm (get_real_problems_and_filter (parse_problem_filters ("--unsolved"::filter_args))))
  | "--solve_real_problems_speculatively!!!"::filter_args ->
      ignore (List.map solve_problem_speculatively_no_confirm (get_real_problems_and_filter (parse_problem_filters ("--unsolved"::filter_args))))
  | ["--reproduce_crash"] ->
      solve_problem test_problem
  | _ ->
      print_string "Unrecognized command line arguments.\n"
;;

main ();;
