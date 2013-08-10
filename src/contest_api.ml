(* I've only been able to run this in an interactive shell so 
   far.  To load this in the interactive shell, run these
   commands:

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

(* Datatype defintions *)

type problem_id = string;;

type oper = Oper1 of op1 | Oper2 of op2 | If0 | Fold | TFold;;

type operator_set = {op1: op1 list; op2: op2 list; if0: bool; fold: bool; tfold: bool};;

type problem = {id: problem_id; size: int; operators: operator_set};;


(* Datatype helper functions *)

let empty_operator_set = {op1 = []; op2 = []; if0 = false; fold = false; tfold = false};;

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
  ^ "  size:      " ^ string_of_int(problem.size) ^ "\n"
  ^ "  operators: " ^ (operator_set_to_string problem.operators) ^ "\n"
  ^ "}\n"
;;


(* Contest API constants*)

let contest_domain = "http://icfpc2013.cloudapp.net";;
let auth_key = "?auth=0191yxaUHzX7C1if61Js0utpeBAUYTCAAlmgdvbAvpsH1H";;

let eval_path  = "/eval";;
let guess_path = "/guess";;
let train_path = "/train";;

let eval_post_url  = contest_domain ^ eval_path  ^ auth_key;;
let guess_post_url = contest_domain ^ guess_path ^ auth_key;;
let train_post_url = contest_domain ^ train_path ^ auth_key;;


(* Helper functions *)

let problem_size n = "{\"size\": " ^ (string_of_int n) ^ "}";;

let send_post post_url post_body =
  let pipeline = new pipeline in
  let post_op = new Http_client.post_raw post_url post_body in
    pipeline#add(post_op);
    pipeline#run();
    post_op#get_resp_body()
;;

let add_to_operator_set op_string operator_set =
  match op_string with
      "not"   -> {op1 = Not :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "shl1"  -> {op1 = Shl1 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "shr1"  -> {op1 = Shr1 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "shr4"  -> {op1 = Shr4 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "shr16" -> {op1 = Shr16 :: operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "and"   -> {op1 = operator_set.op1; op2 = And :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "or"    -> {op1 = operator_set.op1; op2 = Or :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "xor"   -> {op1 = operator_set.op1; op2 = Xor :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "plus"  -> {op1 = operator_set.op1; op2 = Plus :: operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "if0"   -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = true;
                  fold = operator_set.fold; tfold = operator_set.tfold}
    | "fold"  -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = true; tfold = operator_set.tfold}
    | "tfold" -> {op1 = operator_set.op1; op2 = operator_set.op2; if0 = operator_set.if0;
                  fold = operator_set.fold; tfold = true}
    | _       -> invalid_arg ("'" ^ op_string ^ "' is not a known operator.")
;;

let rec parse_operator_set op_list =
  match op_list with
      [] -> empty_operator_set
    | (`String op)::tail -> (add_to_operator_set op (parse_operator_set tail))
    | _ -> invalid_arg "The problem definition's operator list contains an item that does not match (`String _)."
;;

type problem_property = ProblemId of string | ProblemSize of int | ProblemOperators of operator_set | None;;

let parse_problem problem =
  let parse_problem_property property =
    match property with
        ("id", `String id) -> ProblemId id
      | ("size", `Int size) -> ProblemSize size
      | ("operators", `List operators) -> ProblemOperators (parse_operator_set operators)
      | _ -> None in
  let parse_problem_property_list prop_list =
    let rec iter specs property =
      match (parse_problem_property property) with
          ProblemId id -> {id = id; size = specs.size; operators = specs.operators}
        | ProblemSize size -> {id = specs.id; size = size; operators = specs.operators}
        | ProblemOperators ops -> {id = specs.id; size = specs.size; operators = ops}
        | None -> specs in
    List.fold_left iter {id = "-1"; size = -1; operators = empty_operator_set} prop_list in
  print_string problem;
  match from_string(problem) with
      `Assoc problem_spec -> parse_problem_property_list problem_spec
    | _ -> invalid_arg "Problem definition is not properly formatted."
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

let evaluate problem_id inputs =
  let response = send_post eval_post_url (eval_post_body problem_id inputs) in
  Array.of_list (match from_string response with
    `Assoc([("status", `String("ok"));("output", (`List answers))]) ->
      List.map (fun x ->
        match x with
          `String x -> Int64.of_string x
        | _ -> Int64.zero) answers
  | _ -> [])
;;

let guess problem_id program =
   send_post guess_post_url (guess_post_body problem_id program)
;;
