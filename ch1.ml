let id x = x

let compose (g: 'b -> 'c) (f: 'a -> 'b) = fun x -> g (f x)

let compose (g: 'b -> 'c) (f: 'a -> 'b) x =  x |> f |> g

let copy x = string_of_int x ^ string_of_int x

let copy_id = compose copy id

let id_copy = compose id copy

let () = print_endline (copy_id 12)

let () = print_endline (id_copy 12)
