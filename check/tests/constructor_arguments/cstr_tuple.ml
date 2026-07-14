type t =
  | No_param
  | Single_param of (int * float)
  | Multiple_params of int * float
