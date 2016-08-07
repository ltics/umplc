val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val unfold : ('b -> bool) -> ('b -> 'a * 'b) -> 'b -> 'a list
val length : 'a list -> int
val filter : ('a -> bool) -> 'a list -> 'a list
val build_list : (int -> 'a) -> int -> 'a list
val is_empty : 'a list -> bool
val zip : 'a list -> 'b list -> ('a * 'b) list
val map_using_fold : ('a -> 'b) -> 'a list -> 'b list
val map_using_unfold : ('a -> 'b) -> 'a list -> 'b list
val factorial : int -> int
val insert : int -> int list -> int list
val insertion_sort : int list -> int list