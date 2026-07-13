module _ : sig val x : 'a -> 'a end = struct
  external x : 'a -> 'a = "%identity"
end
