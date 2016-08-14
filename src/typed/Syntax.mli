type id = string

type typ =
  | TInt
  | TBool
  | TFun of typ * typ
  | TList of typ

type op2 =
  | LT
  | GT
  | Eq
  | Add
  | Sub
  | Mul

type const =
  | Int of int
  | Bool of bool

type exp =
  | Id of id
  | Const of const
  | Op2 of op2 * exp * exp
  | If of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * typ * exp
  | Fix of id * typ * exp
  | App of exp * exp
  | Empty of typ
  | Cons of exp * exp
  | Head of exp
  | Tail of exp
  | IsEmpty of exp
