open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VList of value list
  | VClosure of id * exp * env

and env = (id * value) list

let typecheck (e : exp) : typ = failwith "not implemented"

let binary_eval (op: op2) (v1: value) (v2: value) : value = match (op, v1, v2) with
  | LT, VInt a, VInt b -> VBool (a < b)
  | GT, VInt a, VInt b -> VBool (a > b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | Eq, VBool a, VBool b -> VBool (a = b)
  | Add, VInt a, VInt b -> VInt (a + b)
  | Sub, VInt a, VInt b -> VInt (a - b)
  | Mul, VInt a, VInt b -> VInt (a * b)
  | _ -> failwith "invalid binary_eval operation"

let binary_subst (op: op2) (v1: exp) (v2: exp) : exp = match (op, v1, v2) with
  | LT, Const (Int a), Const (Int b) -> Const (Bool (a < b))
  | GT, Const (Int a), Const (Int b) -> Const (Bool (a > b))
  | Eq, Const (Int a), Const (Int b) -> Const (Bool (a = b))
  | Eq, Const (Bool a), Const (Bool b) -> Const (Bool (a = b))
  | Add, Const (Int a), Const (Int b) -> Const (Int (a + b))
  | Sub, Const (Int a), Const (Int b) -> Const (Int (a - b))
  | Mul, Const (Int a), Const (Int b) -> Const (Int (a * b))
  | _ -> failwith "invalid binary_eval operation"

let rec subst (x : id) (v : exp) (e : exp) : exp = match e with
  | Id y -> if x = y then v else Id y
  | Const _ -> e
  | Op2 (op, e1, e2) -> Op2 (op, subst x v e1, subst x v e2)
  | If (p, c, a) -> If (subst x v p, subst x v c, subst x v a)
  | Let (y, e1, e2) -> Let (y, subst x v e1, if x = y then e2 else subst x v e2)
  | Fun (y, t, b) -> Fun (y, t, if x = y then b else subst x v b)
  | App (e1, e2) -> App (subst x v e1, subst x v e2)
  | Fix (y, t, b) -> Fix (y, t, if x = y then b else subst x v b)
  | Empty _ -> e
  | Cons (h, t) -> Cons (subst x v h, subst x v t)
  | Head l -> Head (subst x v l)
  | Tail l -> Tail (subst x v l)
  | IsEmpty l -> IsEmpty (subst x v l)

let rec eval_env (env : env) (e : exp) : value = match e with
  | Id x -> List.assoc x env
  | Const c -> (match c with
    | Int i -> VInt i
    | Bool b -> VBool b)
  | Op2 (op, e1, e2) -> binary_eval op (eval_env env e1) (eval_env env e2)
  | If (p, c, a) -> (match (eval_env env p) with
      | VBool b -> (match b with
        | true -> eval_env env c
        | false -> eval_env env a)
      | _ -> failwith "need bool value")
  | Let (x, v, b) -> eval_env ((x, eval_env env v) :: env) b
  | Fun (x, t, b) -> VClosure (x, b, env)
  | App (fn, arg) -> let v = eval_env env arg in
    (match (eval_env env fn) with
     | VClosure (x, b, env') -> eval_env ((x, v) :: env') b
     | _ -> failwith "need closure value")
  (* | Fix (x, t, b) -> eval_env ((x, Fix (x, t, b)) :: env) b *)
  | _ -> failwith "unsupported"

let to_bool (e : exp) : bool = match e with
  | Const (Bool b) -> b
  | _ -> failwith "expected Bool"

let rec eval_subst (e : exp) : exp = match e with
  | Id x -> failwith ("free identifier " ^ x)
  | Const _ -> e
  | Op2 (op, e1, e2) -> binary_subst op (eval_subst e1) (eval_subst e2)
  | If (p, c, a) -> if to_bool (eval_subst p) then eval_subst c else eval_subst a
  | Let (x, v, b) -> eval_subst (subst x (eval_subst v) b)
  | Fun (x, t, b) -> Fun (x, t, eval_subst b)
  | App (e1, e2) -> (match (eval_subst e1, eval_subst e2) with
      | Fun (x, _, b), v -> subst x v b
      | _ -> failwith "expected function")
  | Fix (x, t, b) -> subst x e b
  | Empty _ -> e
  | Cons (e1, e2) -> Cons (eval_subst e1, eval_subst e2)
  | Head l -> (match eval_subst l with
      | Cons (h, _) -> h
      | Empty _ -> failwith "can't get head of empty list"
      | _ -> failwith "expected list")
  | Tail l -> (match eval_subst l with
      | Cons (_, t) -> t
      | Empty _ -> failwith "can't get tail of empty list"
      | _ -> failwith "expected list")
  | IsEmpty l -> (match eval_subst l with
      | Empty _ -> Const (Bool true)
      | Cons _ -> Const (Bool false)
      | _ -> failwith "expected list")

let rec eval (e : exp) : value = match eval_subst e with
  | Const c -> (match c with
    | Int i -> VInt i
    | Bool b -> VBool b)
  | Empty _ -> VList []
  | Cons (h, Empty _) -> VList [eval h]
  | Cons (h, t) -> (match eval t with
      | VList l -> VList (eval h :: l)
      | _ -> failwith "expected list value")
  | _ -> failwith "unexpected term"

(******************************************************************************)
(* The following test cases are part of the assignment document. *)

TEST "normal subtraction associaticity" =
  ParserHelpers.from_string "1 - 2 - 3" =
   Op2 (Sub, Op2 (Sub, Const (Int 1), Const (Int 2)), Const (Int 3))

TEST "a list of three integers" =
  ParserHelpers.from_string "1 :: 2 :: 3 :: empty<int>" =
    Cons(Const (Int 1), Cons (Const (Int 2), Cons (Const (Int 3), Empty TInt)))

TEST "an empty list of booleans" =
  ParserHelpers.from_string "empty<bool>" =
    Empty TBool

TEST "testing the head function" =
  eval (ParserHelpers.from_string "head (1 :: empty<int>)") =
  eval (ParserHelpers.from_string "1")

TEST "testing the tail function" =
  eval (ParserHelpers.from_string "tail (1 :: 2 :: empty<int>)") =
  eval (ParserHelpers.from_string "2 :: empty<int>")

TEST "testing the empty? function (false)" =
  eval (ParserHelpers.from_string "empty? (1 :: 2 :: empty<int>)") =
  eval (ParserHelpers.from_string "false")

TEST "testing the empty? function (true)" =
  eval (ParserHelpers.from_string "empty? empty<int>") =
  eval (ParserHelpers.from_string "true")
