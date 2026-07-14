type empty = |

type var = A | B of int

type _ gadt =
  | A : 'a gadt
  | B : int -> int gadt
