type empty = |

let _ : empty -> 'a = function _ -> .

type _ gadt =
  | Int: int -> int gadt
  | Float: float -> float gadt

let _ = function (_ : string gadt) -> .

let _ = function
  | Int i -> i
  | _ -> .
