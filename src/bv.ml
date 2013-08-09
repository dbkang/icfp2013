open Yojson.Safe;;

type id = string;;

type op1 = Not | Shl1 | Shr1 | Shr4 | Shr16;;

type op2 = And | Or | Xor | Plus;;

type expression =
    ID of id
  | Zero
  | One
  | If0 of expression * expression * expression
  | Fold of expression * expression * id * id * expression
  | Op1 of op1 * expression
  | Op2 of op2 * expression * expression;;


type program = Program of id * expression;;

let program_to_string program = 
  let rec expr_to_string expr =
    match expr with
      ID i -> i
    | Zero -> "0"
    | One -> "1"
    | If0(e1, e2, e3) -> "(if0 " ^ (expr_to_string e1) ^ " " ^
        (expr_to_string e2) ^ " " ^ (expr_to_string e3) ^ ")"
    | Fold(e1, e2, i1, i2, e3) -> "(fold " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^
        " (lambda (" ^ i1 ^ " " ^ i2 ^ ") " ^ (expr_to_string e3) ^ ")" ^ ")"
    | Op1(Not, e) -> "(not " ^ (expr_to_string e) ^ ")"
    | Op1(Shl1, e) -> "(shl1 " ^ (expr_to_string e) ^ ")"
    | Op1(Shr1, e) -> "(shr1 " ^ (expr_to_string e) ^ ")"
    | Op1(Shr4, e) -> "(shr4 " ^ (expr_to_string e) ^ ")"
    | Op1(Shr16, e) -> "(shr16 " ^ (expr_to_string e) ^ ")"
    | Op2(And, e1, e2) -> "(and " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | Op2(Or, e1, e2) -> "(or " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | Op2(Xor, e1, e2) -> "(xor " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
    | Op2(Plus, e1, e2) -> "(plus " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
  in match program with
    Program(i, e) -> "(lambda (" ^ i ^ ") " ^ (expr_to_string e) ^ ")"
;;

