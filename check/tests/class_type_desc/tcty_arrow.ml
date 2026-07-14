class c_no_label : 'a -> object end = fun _ -> object end

class c_label : x:'a -> object end = fun ~x -> c_no_label x

class c_optional : ?x:'a -> object end = fun ?x -> c_no_label x

