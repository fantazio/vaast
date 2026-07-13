module _ = struct
  let x = 0
  let x = 1
end

module type S = sig val x : int end
module _ : S = struct
  let x = 0
  let y = 2
end

module _ : sig val x : int val y : int end = struct
  let y = 0
  let x = 1
end
