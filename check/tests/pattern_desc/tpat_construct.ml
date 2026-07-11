let () = ()
let true = false

type empty = Empty
let Empty = Empty

type 'a single = Single of 'a
let Single Empty = Single Empty

type 'a multiple = Multiple of 'a * 'a
let Multiple (Empty, Empty) = Multiple (Empty, Empty)

type 'a tuple = Tuple of ('a * 'a)
let Tuple (Empty, Empty) = Tuple (Empty, Empty)

type 'a inline_record = Record of { a : 'a }
let Record { a = Empty } = Record { a = Empty }

type gadt =
  | Int : int -> gadt
  | Single : 'a -> gadt
  | Pair : ('a * 'b) -> gadt

let _ = function
  | Int _ -> ()
  | Single (type a) (_ : a) -> ()
  | Pair (type a b) (_, _ : a * b) -> ()
