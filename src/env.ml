type id = string

module Env = struct
  type exp =
    | Id of id
    | Int of int
    | Fun of id * exp
    | App of exp * exp

  type value =
    | VInt of int
    | VClosure of env * id * exp

  and env = (id * value) list

  let rec eval (env : env) (exp : exp) : value = match exp with
    | Id x -> List.assoc x env
    | Int n -> VInt n
    | Fun (x, body) -> VClosure (env, x, body)
    | App (e1, e2)  -> match (eval env e1, eval env e2) with
      | VClosure (env', x, body), v2 -> eval ((x, v2) :: env') body
      | _ -> failwith "expected function"
end

(* high order abstract syntax *)
module HOAS = struct
  type exp =
    | Id of value
    | Int of int
    | Fun of (value -> exp)
    | App of exp * exp

  and value =
    | VInt of int
    | VClosure of (value -> exp)

  let rec eval (exp : exp) : value = match exp with
    | Id v -> v
    | Int n -> VInt n
    | Fun f -> VClosure f
    | App (e1, e2)  -> match (eval e1, eval e2) with
      | VClosure f, v2 -> eval (f v2)
      | _ -> failwith "expected function"
end

module StorePassing = struct
  type exp =
    | Id of id
    | Int of int
    | Fun of id list * exp
    | App of exp * exp list
    | Array of exp list
    | Index of exp * exp
    | Let of id * exp * exp

  type value =
    | VInt of int
    | VPointer of id list * exp
    | VArray of value list

  let rec from_to (lo : int) (hi : int) : int list =
    if lo = hi then [] else lo :: from_to (lo + 1) hi

  let rec realize_env (env : (id * exp) list) : exp * ((id * exp) list)=
    let ixs = from_to 0 (List.length env) in
    let closure = Array (List.map (fun (_, e) -> e) env) in
    let env = List.map2 (fun (x, _) n -> (x, Index (Id "env", Int n))) env ixs in
    (closure, env)

  let rec compile (env : (id * exp) list) (exp : Env.exp) : exp = match exp with
    | Env.Int n -> Int n
    | Env.Id x -> List.assoc x env
    | Env.Fun (x, body) ->
      let (closure, env) = realize_env env in
      Array [closure; Fun (["env"; x], compile ((x, Id x) :: env) body)]
    | Env.App (e1, e2) ->
      Let ("closure", compile env e1,
           App (Index (Id "closure", Int 1),
                [Index (Id "closure", Int 0); compile env e2]))

  let rec eval (env : (id * value) list) (exp : exp) : value = match exp with
    | Id x -> List.assoc x env
    | Int n -> VInt n
    | Fun (args, body) -> VPointer (args, body)
    | App (e1, e2s) -> (match (eval env e1, List.map (eval env) e2s) with
      | VPointer (args, body), v2s ->
        eval (List.map2 (fun x y -> (x, y)) args v2s) body
      | _ -> assert false)
    | Array es -> VArray (List.map (eval env) es)
    | Index (e1, e2) -> (match (eval env e1, eval env e2) with
                         | (VArray vs, VInt n) -> List.nth vs n
                         | _ -> assert false)
    | Let (x, e1, e2) -> eval ((x, eval env e1) :: env) e2
end
