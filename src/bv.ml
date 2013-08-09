open Int64;;

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

module Env = Map.Make(String);;

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


let eval program input =
  let mask = of_int 0xFF in 
  let gen_fold_list v = 
    [logand (shift_right v 56) mask; logand (shift_right v 48) mask; logand (shift_right v 40) mask;
     logand (shift_right v 32) mask;  logand (shift_right v 24) mask; logand (shift_right v 16) mask;
     logand (shift_right v 8) mask; logand v mask] in
  let rec eval_expr expr bindings =
    match expr with
      ID i -> Env.find i bindings
    | Zero -> zero
    | One -> one
    | If0(e1, e2, e3) -> if (eval_expr e1 bindings) = zero then eval_expr e2 bindings else eval_expr e3 bindings
    | Op1(Not, e) -> lognot (eval_expr e bindings)
    | Op1(Shl1, e) -> shift_left (eval_expr e bindings) 1
    | Op1(Shr1, e) -> shift_right_logical (eval_expr e bindings) 1
    | Op1(Shr4, e) -> shift_right_logical (eval_expr e bindings) 4
    | Op1(Shr16, e) -> shift_right_logical (eval_expr e bindings) 16
    | Op2(And, e1, e2) -> logand (eval_expr e1 bindings) (eval_expr e2 bindings)
    | Op2(Or, e1, e2) -> logor (eval_expr e1 bindings) (eval_expr e2 bindings)
    | Op2(Xor, e1, e2) -> logxor (eval_expr e1 bindings) (eval_expr e2 bindings)
    | Op2(Plus, e1, e2) -> add (eval_expr e1 bindings) (eval_expr e2 bindings)
    | Fold(e1, e2, i1, i2, e3) ->
        let func a b = eval_expr e3 (Env.add i1 a (Env.add i2 b bindings)) in
        List.fold_left func (eval_expr e2 bindings) (gen_fold_list (eval_expr e1 bindings))
  in match program with
    Program(i, e) -> eval_expr e (Env.singleton i input)
;;

let size program =
  let rec size_expr expr =
    match expr with
      ID _ | Zero | One -> 1
    | If0(e1, e2, e3) -> 1 + (size_expr e1) + (size_expr e2) + (size_expr e3)
    | Op1(_, e) -> 1 + (size_expr e)
    | Op2(_, e1, e2) -> 1 + (size_expr e1) + (size_expr e2)
    | Fold(e1, e2, _, _, e3) -> 2 + (size_expr e1) + (size_expr e2) + (size_expr e3)
  in match program with
    Program(i, e) -> (size_expr e) + 1
;;


let rec bi_dist n =
  if (n <= 2)
  then [(1, 1)]
  else let rest = bi_dist (n - 1)in
  let first = (List.hd rest) in
  ((fst first), (snd first) + 1)::(List.map (fun (a, b) -> (a + 1, b)) rest)
;;


let rec tri_dist n =
  let bi = bi_dist (n - 1) in
  List.concat (List.map (fun (a, b) -> List.map (fun (x, y) -> (a, x, y)) (bi_dist (b + 1))) bi)
;;

let map_product f al bl =
  List.concat (List.map (fun a -> List.map (f a) bl) al)
;;

let map_product3 f al bl cl =
  List.concat (List.map (fun a -> map_product (f a) bl cl) al)
;;


let gen_programs size op1 op2 if0 fold tfold =
  let id1 = "x" in
  let id2 = "y" in
  let id3 = "z" in
  let rec gen_expr size ids op1 op2 if0 fold =
    let es = Zero::One::(List.map (fun x -> ID x) ids) in
    if (size = 1) then
      es
    else if (size = 2) then
      map_product (fun op e -> Op1(op, e)) op1 es
    else if (size = 3) then 
      let op1e = map_product (fun op e -> Op1(op, e)) op1 (gen_expr 2 ids op1 op2 if0 fold) in
      let op2e = map_product3 (fun op e1 e2 -> Op2(op, e1, e2)) op2 es es in
      List.append op1e op2e
    else if (size = 4) then 
      let op1e = map_product (fun op e -> Op1(op, e)) op1 (gen_expr 3 ids op1 op2 if0 fold) in
      op1e
    else
      []
  in List.map (fun e -> Program(id1, e)) (gen_expr (size - 1) [id1] op1 op2 if0 fold)
;;

