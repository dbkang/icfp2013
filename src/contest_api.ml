(* To load this in the interactive shell, delete the "open Bv;;" line and run:

       #use "topfind";;
       #require "netclient";;
       #require "yojson";;
       #use "src/bv.ml";;
       #use "src/contest_api.ml";;
*)

open Bv;;
open Http_client;;
open String;;
open Yojson.Safe;;

(* Constants *)

let myproblems_cache_filename = "/tmp/myproblems.json";;


(* Datatype defintions *)

type problem_id = string;;

type oper = Oper1 of op1 | Oper2 of op2 | If0 | Fold | TFold;;

type operator_set = {op1: op1 list; op2: op2 list; if0: bool; fold: bool; tfold: bool; bonus: bool};;

type problem = {id: problem_id; size: int; operators: operator_set; solution: string; solved: bool};;

type guess_response = Win | Mismatch of int64 * int64 * int64 | Error of string

(* Datatype helper functions *)

let empty_operator_set = {op1 = []; op2 = []; if0 = false; fold = false; tfold = false; bonus = false};;

let rec join_with_commas str_list =
  match str_list with
      [] -> ""
    | x::[] -> x
    | x::y::z -> x ^ ", " ^ (join_with_commas (y::z))
;;

let operator_set_to_string op_set =
  join_with_commas (List.flatten [
    (List.map op1_to_string op_set.op1);
    (List.map op2_to_string op_set.op2);
    (match op_set.if0 with true -> ["if0"] | false -> []);
    (match op_set.fold with true -> ["fold"] | false -> []);
    (match op_set.tfold with true -> ["tfold"] | false -> []);
  ])
;;

let problem_to_string problem =
    "{\n"
  ^ "  id:        " ^ problem.id ^ "\n"
  ^ "  size:      " ^ (string_of_int problem.size) ^ "\n"
  ^ "  operators: " ^ (operator_set_to_string problem.operators) ^ "\n"
  ^ (match problem.operators.bonus with false -> "" | true -> "  bonus:     true\n")
  ^ (match problem.solution with "" -> "" | solution -> "  solution:  " ^ solution ^ "\n")
  ^ (match problem.solved with false -> "" | true -> "  solved:    true\n")
  ^ "}\n"
;;


(* Contest API constants*)

let contest_domain = "http://icfpc2013.cloudapp.net";;
let auth_key = "?auth=0191yxaUHzX7C1if61Js0utpeBAUYTCAAlmgdvbAvpsH1H";;

let eval_path       = "/eval";;
let guess_path      = "/guess";;
let myproblems_path = "/myproblems";;
let train_path      = "/train";;

let eval_post_url       = contest_domain ^ eval_path       ^ auth_key;;
let guess_post_url      = contest_domain ^ guess_path      ^ auth_key;;
let myproblems_post_url = contest_domain ^ myproblems_path ^ auth_key;;
let train_post_url      = contest_domain ^ train_path      ^ auth_key;;


(* Helper functions *)

let write_string_to_file string filename =
  output_string (open_out filename) string
;;

let read_string_from_file filename =
  input_line (open_in filename)
;;

let send_post post_url post_body =
  let pipeline = new pipeline in
  let post_op = new Http_client.post_raw post_url post_body in
    pipeline#add(post_op);
    pipeline#run();
    post_op#get_resp_body()
;;

let problem_size n = "{\"size\": " ^ (string_of_int n) ^ "}";;

let add_to_operator_set op_string operator_set =
  match op_string with
      "not"   -> {op1 = Not :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "shl1"  -> {op1 = Shl1 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "shr1"  -> {op1 = Shr1 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "shr4"  -> {op1 = Shr4 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "shr16" -> {op1 = Shr16 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "and"   -> {op1 = operator_set.op1; op2 = And :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "or"    -> {op1 = operator_set.op1; op2 = Or :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "xor"   -> {op1 = operator_set.op1; op2 = Xor :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "plus"  -> {op1 = operator_set.op1; op2 = Plus :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "if0"   -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = true;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "fold"  -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = true; tfold = operator_set.tfold; bonus = operator_set.bonus}
    | "tfold" -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = true; bonus = operator_set.bonus}
    | "bonus" -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold; bonus = true}
    | _       -> invalid_arg ("'" ^ op_string ^ "' is not a known operator.")
;;

let rec parse_operator_set op_list =
  match op_list with
      [] -> empty_operator_set
    | (`String op)::tail -> (add_to_operator_set op (parse_operator_set tail))
    | _ -> invalid_arg "The problem definition's operator list contains an item that does not match (`String _)."
;;

type problem_property =
    ProblemId of string
  | ProblemSize of int
  | ProblemOperators of operator_set
  | ProblemSolution of string
  | ProblemSolved of bool
  | None;;

let parse_problem_json problem =
  let parse_problem_property property =
    match property with
        ("id", `String id) -> ProblemId id
      | ("size", `Int size) -> ProblemSize size
      | ("operators", `List operators) -> ProblemOperators (parse_operator_set operators)
      | ("challenge", `String solution) -> ProblemSolution solution
      | ("solved", `Bool solved) -> ProblemSolved solved
      | _ -> None in
  let parse_problem_property_list prop_list =
    let rec iter specs property =
      match (parse_problem_property property) with
          ProblemId id -> {id = id; size = specs.size; operators = specs.operators; solution = specs.solution; solved = specs.solved}
        | ProblemSize size -> {id = specs.id; size = size; operators = specs.operators; solution = specs.solution; solved = specs.solved}
        | ProblemOperators ops -> {id = specs.id; size = specs.size; operators = ops; solution = specs.solution; solved = specs.solved}
        | ProblemSolution solution -> {id = specs.id; size = specs.size; operators = specs.operators; solution = solution; solved = specs.solved}
        | ProblemSolved solved -> {id = specs.id; size = specs.size; operators = specs.operators; solution = specs.solution; solved = solved}
        | None -> specs in
    List.fold_left iter {id = "-1"; size = -1; operators = empty_operator_set; solution = ""; solved = false} prop_list in
    match problem with
      `Assoc problem_spec -> parse_problem_property_list problem_spec
    | _ -> invalid_arg "Problem definition is not properly formatted."
;;

let parse_problem problem =
  parse_problem_json (from_string problem)
;;

let parse_myproblems myproblems_string = 
  let list_of_problems_as_parsed_json = match (from_string myproblems_string) with
    `List x -> x
  | _ -> invalid_arg "Failed to parse the myproblems json." in
  List.map parse_problem_json list_of_problems_as_parsed_json
;;


let eval_post_body problem_id inputs =
    "{"
  ^ "\"id\": \"" ^ problem_id ^ "\", "
  ^ "\"arguments\": [" ^ (join_with_commas (Array.to_list (Array.map (fun x -> "\"" ^ x ^ "\"") inputs))) ^ "]"
  ^ "}"
;;

let guess_post_body problem_id program =
  "{\"id\": \"" ^ problem_id ^ "\", \"program\": \"" ^ (program_to_string program) ^ "\"}"
;;


(* The interesting functions *)

let get_training_problem size =
  parse_problem(send_post train_post_url (problem_size size))
;;

let get_real_problems_from_cache () =
  parse_myproblems (read_string_from_file myproblems_cache_filename)
;;

let get_real_problems_skip_cache () =
  let myproblems_string = (send_post myproblems_post_url "") in
    write_string_to_file myproblems_string myproblems_cache_filename;
    (parse_myproblems myproblems_string)
;;

let get_real_problems () =
  match (Sys.file_exists myproblems_cache_filename) with
      true -> (get_real_problems_from_cache ())
    | false -> (get_real_problems_skip_cache ())
;;

let get_real_problems_and_filter filter_fn =
  match (Sys.file_exists myproblems_cache_filename) with
      true -> (List.filter filter_fn (get_real_problems_from_cache ()))
    | false -> (List.filter filter_fn (get_real_problems_skip_cache ()))
;;

let evaluate problem_id inputs =
  let response = send_post eval_post_url (eval_post_body problem_id inputs) in
  Array.of_list (match from_string response with
    `Assoc([("status", `String("ok"));("outputs", (`List answers))]) ->
      List.map (fun x ->
        match x with
          `String x -> Int64.of_string x
        | _ -> Int64.zero) answers
  | _ -> [])
;;

let guess problem_id program =
  let response = send_post guess_post_url (guess_post_body problem_id program) in
  match from_string response with
    `Assoc([("status", `String("win"))]) -> Win
  | `Assoc([("status", `String("mismatch"));("values", (`List [`String(a); `String(b); `String(c)]))]) ->
      Mismatch((Int64.of_string a), (Int64.of_string b), (Int64.of_string c))
  | `Assoc([("status", `String("error"));("message", `String(a))]) -> Error a
  | _ -> Error "Client-Side Parse Error - Blame Dan"
;;
