class ancestor = object
  method m = ()
end

let _ = object
  inherit ancestor as a
  method m = a#m
end
