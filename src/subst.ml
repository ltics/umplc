type id = string
type exp =
  | Num of int
  | Bool of bool
  | Add of exp * exp
  | Mul of exp * exp
  | GT of exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Id of id

let is_value (e : exp) =
  match e with
  | Bool _ -> true
  | Num _ -> true
  | _ -> false

let to_int (e : exp) : int = match e with
  | Num n -> n
  | _ -> failwith "expected Num"

let to_bool (e : exp) : bool = match e with
  | Bool b -> b
  | _ -> failwith "expected Bool"

let rec subst (x : id) (v : exp) (e : exp) : exp =
  match e with
  | Num n -> Num n
  | Bool b -> Bool b
  | Add (e1, e2) -> Add (subst x v e1, subst x v e2)
  | Mul (e1, e2) -> Mul (subst x v e1, subst x v e2)
  | GT (e1, e2) -> GT (subst x v e1, subst x v e2)
  | If (p, c, a) -> If (subst x v p, subst x v c, subst x v a)
  | Let (y, e1, e2) ->
    Let (y, subst x v e1, if x = y then e2 else subst x v e2)
  | Id y -> if x = y then v else Id y

let rec eval (e : exp) : exp = match e with
  | Num n -> Num n
  | Bool b -> Bool b
  | Add (e1, e2) -> Num (to_int (eval e1) + to_int (eval e2))
  | Mul (e1, e2) -> Num (to_int (eval e1) * to_int (eval e2))
  | GT (e1, e2) -> Bool (to_int (eval e1) > to_int (eval e2))
  | If (e1, e2, e3) -> if to_bool (eval e1) then eval e2 else eval e3
  | Let (x, e1, e2) -> eval (subst x (eval e1) e2)
  | Id x -> failwith ("free identifier " ^ x)
