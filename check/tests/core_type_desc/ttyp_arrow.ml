type t_no_label = int -> int

type t_label = x:int -> t_no_label

type t_optional = ?x:int -> t_no_label
