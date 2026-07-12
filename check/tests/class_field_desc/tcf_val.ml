class p = object
  val v_over = 0
end

class virtual c = object
  inherit p
  val v_conc = 0
  val virtual v_virt : int
  val mutable v_mut = 1
  val! v_over = 2
end
