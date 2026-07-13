class c = object end
type 'a t = #c as 'a

class ['a, 'b] c_poly = object end
type ('a, 'b, 'c) t_poly = ('a, 'b) #c_poly as 'c

(* old syntax *)
type ('a, 'b) variant = [ `A of 'a | `B of 'b ]
type ('a, 'b, 'v) t_var = ('a, 'b) #variant as 'v
