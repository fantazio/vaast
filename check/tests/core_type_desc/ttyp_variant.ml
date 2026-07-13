type t_closed = [ `A | `B of int ]

type 'a t_closed_without_label = [< `A | `B of int ] as 'a

type 'a t_closed_with_labels = [< `A | `B of int | `C > `C `A] as 'a

type 'a t_empty_open = [> ] as 'a

type 'a t_open = [> `A | `B of int ] as 'a
