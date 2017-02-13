(* Maps a function returning optional type over a function, discarding *)
(* those which result in a none result and unwrapping those which are  *)
(* some.                                                               *)
let filter_map f xs =
    let iter xs' accum =
        match xs' with
        | []      -> accum
        | (n::ns) ->
            match (f n) with
            | Some(value) -> iter ns (value::accum)
            | None        -> iter ns accum
    in
    iter xs [];;