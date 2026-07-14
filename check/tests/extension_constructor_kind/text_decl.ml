type 'a t = ..
type 'a t += A of 'a

type 'a t += G : int -> int t
