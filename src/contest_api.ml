(* I've only been able to run this in an interactive shell so 
   far.  To load this in the interactive shell, run these
   commands:

       #use "topfind";;
       #require "netclient";;
       #require "yojson";;
       #use "src/bv.ml";;
       #use "src/contest_api.ml";;
*)

open Http_client;;
open String;;
open Yojson.Safe;;
open Bv;;

(* Datatype defintions *)

type problem_id = string;;

type oper = Oper1 of op1 | Oper2 of op2

type problem = {id: problem_id; size: int; operators: oper list};;


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

let parse_operator_string op_string =
  match op_string with
      "not"   -> Oper1 Not
    | "shl1"  -> Oper1 Shl1
    | "shr1"  -> Oper1 Shr1
    | "shr4"  -> Oper1 Shr4
    | "shr16" -> Oper1 Shr16
    | "and"   -> Oper2 And
    | "or"    -> Oper2 Or
    | "xor"   -> Oper2 Xor
    | "plus"  -> Oper2 Plus
    | _       -> invalid_arg ("'" ^ op_string ^ "' is not a known operator.")
;;

let rec parse_operator_list op_list =
  match op_list with
      [] -> []
    | (`String op)::tail -> (parse_operator_string op)::(parse_operator_list tail)
    | _ -> invalid_arg "The problem definition's operator list contains an item that does not match (`String _)."

type problem_property = ProblemId of string | ProblemSize of int | ProblemOperators of oper list | None;;

let parse_problem_property property =
  match property with
      ("id", `String id) -> ProblemId id
    | ("size", `Int size) -> ProblemSize size
    | ("operators", `List operators) -> ProblemOperators (parse_operator_list operators)
    | _ -> None
;;

let parse_problem_property_list prop_list =
  let rec iter specs property =
    match (parse_problem_property property) with
        ProblemId id -> {id = id; size = specs.size; operators = specs.operators}
      | ProblemSize size -> {id = specs.id; size = size; operators = specs.operators}
      | ProblemOperators ops -> {id = specs.id; size = specs.size; operators = ops}
      | None -> specs in
  List.fold_left iter {id = "fake"; size = 0; operators = []} prop_list
;;

let parse_problem problem =
  match from_string(problem) with
      `Assoc problem_spec -> parse_problem_property_list problem_spec
    | _ -> invalid_arg "Problem definition is not blah blah blah."
;;

(* The interesting functions *)

let get_training_problem size =
  parse_problem(send_post train_post_url (problem_size size))
;;

let guess problem_id program =
   send_post guess_post_url ("{\"id\": \"" ^ problem_id ^ "\", \"program\": \"" ^ (program_to_string program) ^ "\"}")
;;

