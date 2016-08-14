open Syntax

(* TODO: Add cases to value *)
type value

(* TODO: Implement typecheck and any required helper functions. *)
let typecheck (e : exp) : typ = failwith "not implemented"

(* TODO: Implement eval and any required helper functions. *)
let eval (e : exp) : value = failwith "not implemented"


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