let sleep_id n =
  Unix.sleep n;
  n

let memoize f =
  let cache = ref [] in
    fun x ->
      try List.assoc x !cache with
        Not_found -> 
          let res = f x in
            cache := (x, res) :: !cache;
            res

let m_sleep_id = memoize sleep_id

let m_int = memoize Random.int

let seed_int seed = Random.init seed; Random.int

let m_seed_int = memoize seed_int

let id x = x

let same :bool -> bool = fun x ->
  match x with
    true -> true
  | false -> false

let neg :bool -> bool = fun x ->
  match x with
    true -> false
  | false -> true

let always_true: bool -> bool = fun x -> true

let always_false: bool -> bool = fun x -> false
