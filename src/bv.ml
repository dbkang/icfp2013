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
  | Op2 of op2 * expression * expression
  | GenericOp1 of expression * int ref
  | GenericOp2 of expression * expression * int ref
;;

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
    | GenericOp1(e, _) -> "(op1 " ^ (expr_to_string e) ^ ")"
    | GenericOp2(e1, e2, _) -> "(op2 " ^ (expr_to_string e1) ^ " " ^ (expr_to_string e2) ^ ")"
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
    | _ -> zero
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
    | GenericOp1(e, _) -> 1 + (size_expr e)
    | GenericOp2(e1, e2, _) -> 1 + (size_expr e1) + (size_expr e2)
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

let bi_dist_left n =
  List.filter (fun (a, b) -> a <= b) (bi_dist n)
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

(* like map_product over the same list, but generates unique combinations *)
let self_map_left f al =
  let rec self_map_rec l result =
    match l with
      [] -> result
    | hd::tl -> self_map_rec tl (List.append (List.map (f hd) l) result) in
  self_map_rec al []
;;

let repeat x n =
  let rec repeat_partial m partial =
    if (m = n) then partial else repeat_partial (m + 1) (x::partial) in
  repeat_partial 0 []
;;

let rec range m n = if (m > n) then [] else m::(range (m + 1) n)
;;

let rec except_nth l n = 
  match (l, n) with
    (hd::tl, 0) -> tl
  | (hd::tl, n) -> hd::(except_nth tl (n - 1))
  | ([], _) -> []
;;

let gen_pseudo size if0 fold tfold =
  let id1 = "x" in
  let id2 = "y" in
  let id3 = "z" in
  let rec if0_dist if0 n =
    match (if0, n) with
      (_, 1) -> [[if0]]
    | (false, n) -> [repeat false n]
    | (true, n) -> List.append (List.map (fun l -> true::l) (if0_dist false (n - 1)))
          (map_product (fun x l -> x::l) [true; false] (if0_dist true (n - 1))) in
  let rec fold_dist fold n =
    match (fold, n) with
      (_, 1) -> [[fold]]
    | (false, n) -> [repeat false n]
    | (true, n) ->
        let partial = List.map (fun l -> false::l) (fold_dist true (n - 1)) in
        (true::(repeat false (n - 1)))::partial in
  let rec gen_expr size ids if0 fold =
    let es = Zero::One::(List.map (fun x -> ID x) ids) in
    let op1es_calc n =
      List.map (fun e -> GenericOp1(e, ref 0)) (gen_expr n ids if0 fold) in
    let op2es_calc_partial n if01 if02 fold1 fold2 =
      if (n mod 2 = 1 || if01 != if02 || fold1 != fold2) then (* bi_dist is sufficient to ensure uniqueness *)
        List.concat (List.map (fun (n1, n2) ->
          map_product (fun e1 e2 -> GenericOp2(e1, e2, ref 0))
            (gen_expr n1 ids if01 fold1)
            (gen_expr n2 ids if02 fold2)) (bi_dist_left n))
      else
        List.concat (List.map (fun (n1, n2) ->
          if n1 = n2 then
            self_map_left (fun e1 e2 -> GenericOp2(e1, e2, ref 0)) (gen_expr n1 ids if01 fold1)
          else
            map_product (fun e1 e2 -> GenericOp2(e1, e2, ref 0))
              (gen_expr n1 ids if01 fold1)
              (gen_expr n2 ids if02 fold2)) (bi_dist_left n)) in
    let op2es_calc n = List.concat
        (map_product (fun if0n foldn ->
          op2es_calc_partial n (List.hd if0n) (List.nth if0n 1) (List.hd foldn) (List.nth foldn 1))
           (if0_dist if0 2) (fold_dist fold 2)) in
    
    let op3es_calc_partial f n ids if01 if02 if03 fold1 fold2 fold3 =
      List.concat (List.map (fun (n1, n2, n3) ->
        map_product3 f
          (gen_expr n1 ids if01 fold1)
          (gen_expr n2 ids if02 fold2)
          (gen_expr n3 ids if03 fold3)) (tri_dist n)) in
    
    let op3es_calc f n ids if0 fold = List.concat
        (map_product (fun if0n foldn ->
          let (if0ni, foldni) = (List.nth if0n, List.nth foldn) in
          op3es_calc_partial f n ids (if0ni 0) (if0ni 1) (if0ni 2) (foldni 0) (foldni 1) (foldni 2))
           (if0_dist if0 3) (fold_dist fold 3)) in
        
    let if0_gen e1 e2 e3 = If0(e1, e2, e3) in
    let fold_gen idx idy e1 e2 e3 = Fold(e1, e2, idx, idy, e3) in

    match (size, if0, fold) with 
      (1, false, false) -> es
    | (2, false, false) -> List.map (fun e -> GenericOp1(e, ref 0)) es
    | (3, false, false) -> List.append (op1es_calc 2) (op2es_calc 2)
    | ((1 | 2 | 3), _, _) -> []
    | (4, _, true) -> []
    | (4, true, _) -> op3es_calc if0_gen 3 ids false false
    | (4, false, false) -> List.append (op1es_calc 3) (op2es_calc 3)
    | (5, true, true) -> []
    | (5, false, true) ->
        List.append (op3es_calc (fold_gen id1 id2) 3 [id1;id2] false false)
          (op3es_calc (fold_gen id2 id3) 3 (id2::id3::ids) false false)
    | (5, if0, false) ->  List.concat [(op1es_calc 4);
                                       (op2es_calc 4);
                                       if if0 then (op3es_calc if0_gen 4 ids false false) else []]
    | (n, if0, fold) ->
        List.concat [(op1es_calc (n - 1));
                     (op2es_calc (n - 1));
                     if if0 then
                       List.append (op3es_calc if0_gen (n - 1) ids true fold)
                         (op3es_calc if0_gen (n - 1) ids false fold)
                     else [];
                     if fold then (op3es_calc (fold_gen id1 id2) (n - 2) [id1;id2] if0 false) else [];
                     if fold then (op3es_calc (fold_gen id2 id3) (n - 2) (id2::id3::ids) if0 false) else []]
  in if tfold then
    List.map (fun e -> Program(id1, Fold(ID(id1), Zero, id1, id2, e)))
      (gen_expr (size - 4) [id1; id2] if0 false)
  else
    List.map (fun e -> Program(id1, e)) (gen_expr (size - 1) [id1] if0 fold)
;;


(* Used for both op1 and op2 *)
let rec gen_op_combinations ops op_count =
  if (List.length ops) > op_count then
    []
  else if (op_count = 0) then
    [[]]
  else
    List.concat
      (List.map (fun n -> List.append
          (List.map (fun l -> (List.nth ops n)::l) (gen_op_combinations (except_nth ops n) (op_count - 1)))
          (List.map (fun l -> (List.nth ops n)::l) (gen_op_combinations ops (op_count - 1))))
         (range 0 ((List.length ops) - 1)))
;;

let gen_programs op1s op2s (Program(id, pseudo)) =
  let op1_count = ref 0 in
  let op2_count = ref 0 in
  let rec count_and_mark expr =
    match expr with
      If0(e1, e2, e3) -> count_and_mark e1; count_and_mark e2; count_and_mark e3; ()
    | Fold(e1, e2, _, _, e3) -> count_and_mark e1; count_and_mark e2; count_and_mark e3; ()
    | GenericOp1(e1, i) -> i := !op1_count; op1_count := !op1_count + 1; count_and_mark e1; ()
    | GenericOp2(e1, e2, i) -> i := !op2_count; op2_count := !op2_count + 1; count_and_mark e1; count_and_mark e2; ()
    | _ -> () in
  let _ = count_and_mark pseudo in
  let op1_combinations = gen_op_combinations op1s !op1_count in
  let op2_combinations = gen_op_combinations op2s !op2_count in
  let rec apply_comb expr op1_comb op2_comb =
    let apply expr = apply_comb expr op1_comb op2_comb in
    match expr with
      If0(e1, e2, e3) -> If0(apply e1, apply e2, apply e3)
    | Fold(e1, e2, i1, i2, e3) -> Fold(apply e1, apply e2, i1, i2, apply e3)
    | GenericOp1(e, i) -> Op1((List.nth op1_comb !i), (apply e))
    | GenericOp2(e1, e2, i) -> Op2((List.nth op2_comb !i), (apply e1), (apply e2))
    | e -> e in
  List.map (fun e -> Program(id, e)) (map_product (apply_comb pseudo) op1_combinations op2_combinations)
;;

let gen_programs_all size op1s op2s if0 fold tfold =
  Array.concat (List.map (fun p -> Array.of_list (gen_programs op1s op2s p)) (gen_pseudo size if0 fold tfold))
;;

(* create 256 random arguments *)
let gen_arguments () =
  Array.map (fun a -> Random.int64 max_int) (Array.init 255 (fun i -> i))
;;

let pregen_arguments = gen_arguments () ;;

let solver size op1s op2s if0 fold tfold =
  let candidates = gen_programs_all size op1s op2s if0 fold tfold in
  let answers = Array.map (fun p -> Array.map (eval p) pregen_arguments) candidates in
  answers; ();
;;

