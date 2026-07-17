class c = object end
type 'a t = #c as 'a

class ['a, 'b] c_poly = object end
type ('a, 'b, 'c) t_poly = ('a, 'b) #c_poly as 'c


#if OCAML_VERSION < (5, 1, 0)
(* old syntax *)
type ('a, 'b) variant = [ `A of 'a | `B of 'b ]
type ('a, 'b, 'v) t_var = ('a, 'b) #variant as 'v
#elif OCAML_VERSION >= (5, 1, 0)
(* The #variant is now a compilation error: "Unbound class type variant" *)
#endif
