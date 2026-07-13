type t = ..
type t +=
  | Foo
  | Bar of int

type t += private
  | Qux
