open Core.Std

(* Do not change this definition *)
let rec foldr (f : 'a -> 'b -> 'b) (acc: 'b) (lst : 'a list) : 'b =
  match lst with
  | [] -> acc
  | hd :: tl -> f hd (foldr f acc tl)

(* Do not change this definition *)
let rec unfold (p : 'b -> bool) (g : 'b -> 'a * 'b) (acc : 'b) : 'a list =
  match p acc with
    | true -> []
    | false ->
      let (a, acc') = g acc in
      a :: (unfold p g acc')

let length (lst : 'a list) : int =
  foldr (fun _ acc -> acc + 1) 0 lst

let filter (pred : 'a -> bool) (lst : 'a list) : 'a list =
  foldr (fun e acc -> match pred e with
      | true -> e :: acc
      | false -> acc) [] lst

let reverse (lst : 'a list) : 'a list =
  foldr (fun e acc -> acc @ [e]) [] lst

let take (n: int) (lst : 'a list) : 'a list =
  reverse (foldr (fun e acc -> if length acc = n
                   then acc
                   else e :: acc) [] (reverse lst))

let build_list (f : int -> 'a) (len : int) : 'a list =
  reverse (unfold (fun x -> x = 0) (fun x -> (f x, x - 1)) len)

let is_empty (lst : 'a list) : bool =
  foldr (fun _ _ -> false) true lst

let zip (lst1 : 'a list) (lst2 : 'b list) : ('a * 'b) list =
  let (lst1, lst2) = match length lst1 < length lst2 with
                          | true -> (lst1, take (length lst1) lst2)
                          | false -> (take (length lst2) lst1, lst2)
  in fst (foldr (fun e (acc, l) -> match l with
      | [] -> (acc, l)
      | h :: t -> ((e, h) :: acc, t)) ([], reverse lst2) lst1)

let map_using_fold (f : 'a -> 'b) (lst : 'a list) : 'b list =
  foldr (fun e acc -> f e :: acc) [] lst

let map_using_unfold (f : 'a -> 'b) (lst : 'a list) : 'b list =
  unfold (fun x -> x = []) (fun x -> match x with
      | h :: t -> (f h, t)
      | [] -> failwith "could not be empty") lst

let factorial n =
  foldr (fun e acc -> e * acc) 1 (unfold (fun x -> x = 0) (fun x -> (x, x - 1)) n)

let (<~) f g x = f(g(x))
let (~>) f g x = g(f(x))

let insert (x : int) (lst : int list) : int list =
  match is_empty lst with
  | true -> [x]
  | false ->
    let r = fst (foldr (fun e (acc, flag) -> match x >= e && flag with
      | true -> (e :: x :: acc, false)
      | false -> (e :: acc, flag)) ([], true) lst)
    in match length r = length lst with
    | true -> x :: r
    | false -> r

let insertion_sort (xs : int list) =
  foldr (fun e acc -> insert e acc) [] xs
