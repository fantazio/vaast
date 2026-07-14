type 'a t = [<
  | `A
  | `B of int
  | `C of int & string
  | `D of & int
] as 'a
