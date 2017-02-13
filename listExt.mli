(* Maps a function returning optional type over a function, discarding *)
(* those which result in a none result and unwrapping those which are  *)
(* some.                                                               *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list