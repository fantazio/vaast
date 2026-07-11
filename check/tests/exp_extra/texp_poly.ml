let _ = object
  method m = ()
end

let _ = object
  method m : 'a -> unit = ignore
end

let _ = function x -> object
  method m : 'a = x
end
