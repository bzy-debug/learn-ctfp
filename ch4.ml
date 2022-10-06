type 'a writer = 'a * string

let (>=>) m1 m2 = fun x ->
  let (y, s1) = m1 x in
  let (z, s2) = m2 y in
    (z, s1 ^ s2)

let id x = (x, "")

let to_upper s = (String.map Char.uppercase_ascii s, "to_upper ")

let to_words s = (String.split_on_char ' ' s, "to_words ")

let process = to_upper >=> to_words

type 'a optional = {is_valid: bool; value: 'a}

let safe_root x =
  if x >= 0.
  then {is_valid = true; value = sqrt x}
  else {is_valid = false; value = x}

let safe_reciprocal x =
  if x <> 0.
  then {is_valid = true; value = 1. /. x}
  else {is_valid = false; value = x}

let compose m2 m1 = fun x ->
  let {is_valid = v1; value = val1} = m1 x in
  let {is_valid = v2; value = val2} = m2 val1 in
    {is_valid = v1 && v2; value = val2}

let id x = {is_valid = true; value = x}

let safe_root_reciprocal = compose safe_root safe_reciprocal
